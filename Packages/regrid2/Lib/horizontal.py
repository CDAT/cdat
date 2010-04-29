## Automatically adapted for numpy.oldnumeric Aug 02, 2007 by 

import numpy
import cdms2
import  _regrid, string, copy
from error import RegridError

_debug = 0                              # Set to 1 for debug

# Map (n,2) boundary arrays to individual boundary arrays. Returns (lowerBounds, upperBounds)
def extractBounds(bounds):
    if bounds[0,0] < bounds[0,1]:
        lower = bounds[:,0]
        upper = bounds[:,1]
    else:
        lower = bounds[:,1]
        upper = bounds[:,0]

    return (lower.astype(numpy.float32), upper.astype(numpy.float32))

# Create a regridder. ingrid and outgrid are CDMS AbstractGrid objects.
class Regridder:

    def __init__(self, ingrid, outgrid):

        inlat = ingrid.getLatitude()
        outlat = outgrid.getLatitude()
        inlon = ingrid.getLongitude()
        outlon = outgrid.getLongitude()
        inlatBounds, inlonBounds = ingrid.getBounds()
        outlatBounds, outlonBounds = outgrid.getBounds()

        self.nlati = len(inlat)
        self.nlato = len(outlat)
        self.nloni = len(inlon)
        self.nlono = len(outlon)
        self.inmask = ingrid.getMask()
        self.outmask = outgrid.getMask()
        # Make grid masks consistent with 'internal' convention:
        # 0 == invalid
        if self.inmask is not None:
            self.inmask = 1. - self.inmask
        if self.outmask is not None:
            self.outmask = 1. - self.outmask
        self.inshape = ingrid.shape
        self.inorder = ingrid.getOrder()
        self.outlat = outgrid.getLatitude().clone()
        self.outlon = outgrid.getLongitude().clone()

        bsin, bnin = extractBounds(inlatBounds)
        bwin, bein = extractBounds(inlonBounds)
        bsout, bnout = extractBounds(outlatBounds)
        bwout, beout = extractBounds(outlonBounds)

        if _debug==1:
            import sys
            sys.stdout = open('debug_regrid.txt','w')
            print "bsin = ", numpy.array2string(bsin,precision=3)
            print "bnin = ", numpy.array2string(bnin,precision=3)
            print "bwin = ", numpy.array2string(bwin,precision=3)
            print "bein = ", numpy.array2string(bein,precision=3)
            print "bsout = ", numpy.array2string(bsout,precision=3)
            print "bnout = ", numpy.array2string(bnout,precision=3)
            print "bwout = ", numpy.array2string(bwout,precision=3)
            print "beout = ", numpy.array2string(beout,precision=3)

        self.londx, self.lonpt, self.wtlon, self.latdx, self.latpt, self.wtlat = _regrid.maparea( self.nloni, self.nlono, self.nlati, self.nlato, bnin, bnout, bsin, bsout, bein, beout, bwin, bwout )

    # Call the regridder function.
    # ar is the input array.
    # order is of the form "tzyx", "tyx", etc.
    # missing is the missing data value, if any.
    # mask is either 2-D or the same shape as ar.
    # If returnTuple is true, return the tuple (outArray, outWeights) where
    # outWeights is the fraction of each zone of the output grid which overlaps non-missing
    # zones of the input grid; it has the same shape as the output array.
    def __call__(self, ar, missing=None, order=None, mask=None, returnTuple=0):

        import cdms2
        from cdms2.avariable import AbstractVariable
        from cdms2.tvariable import TransientVariable

        # Compatibility
        if mask is numpy.ma.nomask:
            mask = None

        if ar.dtype.type is numpy.bool_:
            ar = numpy.asarray(ar, numpy.float32)

        # Make sense of mask consistent with 'internal' convention (0 == invalid)
        if mask is not None:
            mask = 1. - mask

        # Save Variable metadata for output
        if isinstance(ar, AbstractVariable):
            attrs = copy.copy(ar.attributes)
            varid = ar.id
            axislist = list(map(lambda x: x[0].clone(), ar.getDomain()))
            inputIsVariable = 1
            if order is None:
                order = ar.getOrder()
            #this expects contiguous arrays
            if isinstance(ar, TransientVariable) and ar.iscontiguous() is False:
                ar = ar.ascontiguous()
            
        else:
            inputIsVariable = 0

        # Turn ar into a numpy array.
        if numpy.ma.isMaskedArray(ar):
            armiss = ar.fill_value
            armask = ar.mask
            if armask is numpy.ma.nomask:
                armask = None
            else:
                armask = 1. - armask # Reverse numpy.ma convention for rgdarea
            ar = numpy.ma.filled(ar)
        elif isinstance(ar, AbstractVariable):
            tempar = ar.getValue(squeeze=0)
            armiss = ar.getMissing()
            armask = tempar.mask
            if armask is numpy.ma.nomask:
                armask = None
            else:
                armask = 1. - armask # Reverse numpy.ma convention for rgdarea
            ar = numpy.ma.filled(tempar)
        elif isinstance(ar, numpy.ndarray):
            armask = armiss = None
        else:
            raise RegridError, "Input array is not a Variable, numpy.ma, or numpy array"
        
        # If neither mask nor missing value is specified, get them from
        # the input array.
        if mask is None and missing is None:
            missing = armiss
            mask = armask
        if isinstance(missing, numpy.ndarray):
            missing = missing[0]

        rank = len(ar.shape)
        assert 2<=rank<=4, 'Array rank is %i, must be 2, 3, or 4'%rank

        # Set the default order to match the input grid order
        if order is None:
            if rank==2:
                order = self.inorder
            elif rank==3:
                order = "t"+self.inorder
            else:
                order = "tz"+self.inorder
        
        assert rank==len(order), 'Order must be same length as array rank: %i'%len(ar.shape)

        order = string.lower(order)

        # Map order to ilon, ilat ...
        itim1 = itim2 = 0
        ilon = ilat = -1
        idim = 0
        for i in range(rank-1,-1,-1):
            c = order[i]
            if c=='x':
                ilon = idim
            elif c=='y':
                ilat = idim
            elif c=='z':
                itim1 = idim
            elif c=='t':
                if rank==3:
                    itim1 = idim
                else:
                    itim2 = idim
            idim = idim+1

        # Map array shape to nloni, nlati, ...
        ntim1 = ntim2 = 0
        shape = ar.shape
        if ilon==-1:
            raise RegridError, "Input grid does not have a longitude axis"
        if ilat==-1:
            raise RegridError, "Input grid does not have a latitude axis"
        nlati = shape[rank-ilat-1]
        nloni = shape[rank-ilon-1]
        if nlati!=self.nlati or nloni!=self.nloni:
            raise ShapeError, 'array lat,lon (%i,%i) does not match grid lat,lon (%i,%i)'%(nlati, nloni, self.nlati, self.nloni)

        if itim1!=0: ntim1 = shape[rank-itim1-1]
        if itim2!=0: ntim2 = shape[rank-itim2-1]

        # Construct the input mask:
        # If user mask is 2-D or not specified, use the logical AND of the mask (or input grid mask
        # if no user mask is specified)
        # with the 'implicit mask' generated from the missing data, if any
        if (mask is None) or len(mask.shape)==2:
            flag2D = 1
            if mask is not None:
                assert mask.shape==self.inshape, '2-D mask must be same shape as input grid'
                inmask = mask
            elif self.inmask is None:
                inmask = numpy.ones(self.inshape)
            else:
                inmask = self.inmask

            if missing is not None:
                if rank==2:
                    firstslice = ar
                elif rank==3:
                    firstslice = ar[0]
                else:
                    firstslice = ar[0,0]
                # inmask = numpy.logical_and( numpy.greater( numpy.absolute( firstslice - missing), numpy.absolute( 0.001*missing)), inmask)
                if issubclass(ar.dtype.type, numpy.floating):
                    inmask = numpy.where( numpy.greater( numpy.absolute( firstslice - missing), numpy.absolute( 0.001*missing)), inmask, 0)

        # If the user mask was specified and is > 2-D, it overrides the grid mask
        else:
            assert mask.shape==ar.shape, 'Mask must be 2-D or same shape as input array'
            inmask = mask
            flag2D = 0                  # 2-D user masks are handled above
            # If armask is derived from the input array, it is probably consistent
            # with the missing value - don't bother recalculating it
            if missing is not None and armask is None:
                # inmask = numpy.logical_and( numpy.greater( numpy.absolute( ar - missing), numpy.absolute( 0.001*missing)), inmask)
                if issubclass(ar.dtype.type, numpy.floating):
                    inmask = numpy.where( numpy.greater( numpy.absolute( ar - missing), numpy.absolute( 0.001*missing)), inmask, 0)
        # Cast the mask to float
        inmask = inmask.astype(numpy.float32)
        if missing is None: missing=1.0e20

        # Cast the input array to 32-bit floats, if necessary
        if ar.dtype.char != numpy.float32:
            ar = ar.astype(numpy.float32)

        # Malloc return array
        outshape = list(shape)
        outshape[rank-ilat-1] = self.nlato
        outshape[rank-ilon-1] = self.nlono
        outar = numpy.zeros(tuple(outshape),numpy.float32)

        # Perform the regridding. The return array has the same shape as the output array, and is the fraction of the zone which overlaps a non-masked zone of the input grid.
        amskout = _regrid.rgdarea(ilon, ilat, itim1, itim2, ntim1, ntim2, nloni, self.nlono, nlati, self.nlato, flag2D, missing, self.londx, self.lonpt, self.wtlon, self.latdx, self.latpt, self.wtlat, inmask, ar, outar)

        # Correct the shape of output weights
        amskout.shape = outar.shape

        # Set the missing data mask of the output array, if any.
        hasMissing = not numpy.ma.alltrue(numpy.ma.ravel(amskout))
        if hasMissing:
            slabMask = numpy.ma.where(numpy.ma.equal(amskout, 0), 1, 0)
        else:
            slabMask = None

        # Combine missing data mask and output grid mask
        # Note: slabMask and outmask are Boolean here
        if self.outmask is not None:
            outmask = numpy.logical_not(numpy.resize(self.outmask, outshape))
            if hasMissing:
                outmask = numpy.ma.logical_or(outmask, slabMask)
        else:
            outmask = slabMask

        # Create the result TransientVariable (if input ar is an AbstractVariable)
        # or masked array
        if inputIsVariable==1:
            for i in range(len(order)):
                if order[i]=='x':
                    axislist[i] = self.outlon
                elif order[i]=='y':
                    axislist[i] = self.outlat
            result = cdms2.createVariable(outar, mask = outmask, fill_value = missing,
                                         axes = axislist, attributes = attrs, id = varid)
        else:
            result = numpy.ma.masked_array(outar, mask = outmask, fill_value = missing)
            
        if returnTuple==0:
            return result
        else:
            return result, amskout

def input_mask(ain, type,  mask, missing = None):

    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: set up the input mask including missing from ain
    #
    #     usage:    
    #
    #     passed : 
    #
    #     returned:  
    #
    #
    #------------------------------------------------------------------------"""
    if type != 'h' and type != 'v':
        raise ValueError, 'Mask type must be h or v'
        return 

    if missing == None:
        try:
            omit = ain.missing_value
        except AttributeError:
            omit = 1.0e20
    else:
        omit = missing

    # ----- insert 0.0 in mask where array has missing data -------

    mask_size = len(mask.shape)
    data_size = len(ain.shape)

    if mask_size ==  2 and data_size > 2:             # make reduced array with first lat_lon section from a

        if data_size == 3:                            # caution: assuming standard order lat-lon varying the fastest
            if type == 'h':
                reduced = ain[0,:,:] 
            elif type == 'v':
                reduced = ain[:,:,0]                  # removes lats dummy latitude
        elif data_size == 4:                       
            if type == 'h':
                reduced = ain[0,0,:,:] 
            elif type == 'v':
                reduced = ain[0,:,:,0]                # removes lats dummy latitude
        else:
            raise IndexError, 'Data size is out of range'
            return 
         
        amskin = numpy.where( numpy.greater(reduced, 0.9*omit),  0.0, mask)
        amskin = amskin.astype(numpy.float32)

    else:                                                    # 0.0 -> missing in passed mask

        amskin = numpy.where( numpy.greater(ain, 0.9*omit),  0.0, mask)
        amskin = amskin.astype(numpy.float32)

    return omit, amskin  

