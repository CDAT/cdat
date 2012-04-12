import numpy, copy
import cdms2
import regrid2
from regrid2 import RegridError
import re

esmfImported = False
try:
    from regrid2 import esmf
    import ESMP
    esmfImported = True
except:
    pass

def _setMaskDtype(mask):
    """
    Convert a mask to int32
    @param mask the mask to be converted
    """
    if mask.dtype != numpy.int32:
        if mask.dtype == numpy.bool:
            mask = numpy.array(mask, numpy.int32)
        elif mask.dtypye == numpy.bool8:
            mask = numpy.array(mask, numpy.int32)
            
    return mask

def _makeGridList(grid):
    """
    Convert a cdms2 grid to a list of coordinates
    @param grid The grid to be converted
    """
    if cdms2.isGrid(grid):
        index = 0
        retGrid = [grid.getLatitude(), grid.getLongitude()]
        try:
            retGrid.append(grid.getLevel())
        except:
            pass
        rank = len(retGrid)
            
    elif isinstance(grid, list):
        rank = len(grid)
        retGrid = grid
    else:
        raise RegridError, 'Grid must be a list of coordinates or a cdms2 grid'
    return retGrid, rank


class Regridder:
    def __init__(self, inGrid, outGrid, mask = None, regridTool = "gsRegrid",
                 regridMethod = "bilinear", **args):
        """
        Constructor for regridding object. Currently just uses a 2-D object
        @param ingrid cdms2, ndarray variable
        @param outGrid cdms2, ndarray variable
        @param mask The mask
        @param regridTool Which regridder to use. 
                        regrid2 - uses only axis CDAT original regriding tool
                        gsRegrid - curviliner grids - libcf is a synonym
                        esmf - curvilinear grids, axes, bilinear, conservative.
                               ESMP is a synonym.
                        scrip - many options but requires a remapping file from
                                SCRIP
        @param regridMethod Conservative, Mutlilinear. Some regrid tools use 
                            only bilinear or multilinear
        @param **args Optional keyword arguments for each type of regridder
                gsRegrid accepts nitermax and tolpos for computing the weights
        """
        if regridTool is None:
            regridTool = 'gsregrid'
        if regridMethod is None:
            regridMethod = 'bilinear'
        rgTool = regridTool.lower()
        rgMeth = regridMethod.lower()
        self.regridTool = rgTool
        self.regridMethod = rgMeth
        self.outmask = None
        
        if re.match('regrid', rgTool, re.I):
            self.regridObj = regrid2.Horizontal(ingrid, outGrid)
            self.regridTool = 'regrid2'
        elif rgTool == 'scrip':
            pass
        elif re.match('esm', rgTool):
            self.esmfImported = esmfImported
            if not self.esmfImported:
                string = "ESMP is not installed or is unable to be imported"
                raise RegridError, string
            self.regridTool = 'esmp'

            # Set the regridding method - Bilinear / Conservative
            if regridMethod is None or regridMethod.lower() == 'bilinear':
                self.regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR
            else:
                self.regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE
            if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
                # If the bounds are the same rank maybe we can use them...
                if inGrid.getBounds().rank() == inGrid.rank():
                    pass                
                else:
                    message = \
                """\n
             Conservative regridding not implemented in regrid2 for coordinates
             whose ranks are not the same as the bounds
             import ESMP
             import esmf
             Then create the coordinates from the bounds for both the 
             source and destination grids.
                """
                    raise RegridError, message
            

            if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_BILINEAR:
                self.location = ESMP.ESMP_MESHLOC_NODE
            elif self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
                self.location = ESMP.ESMP_MESHLOC_ELEMENT

            self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
            for arg in args:
                if re.search('unmappedaction', args[arg]):
                    unMappedAction = args[arg]
                    if re.search('error', unMappedAction):
                        self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_ERROR

            # Initialize the ESMP
            esmf.initialize()
            srcGrid, self.srcRank = _makeGridList(inGrid)
            dstGrid, self.dstRank = _makeGridList(outGrid)
            self.srcMesh = esmf.EsmfStructMesh(srcGrid)
            self.dstMesh = esmf.EsmfStructMesh(dstGrid)

        elif rgTool == 'gsregrid' or rgTool == 'libcf':
            # Prep in and out grid for gsRegrid!
            self.srcGrid, self.srcRank = _makeGridList(inGrid)
            self.dstGrid, self.dstRank = _makeGridList(outGrid)

            if self.dstRank != self.srcRank:
                raise RegridError, 'outGrid rank (%d) != inGrid rank (%d)' % \
                                   (self.dstRank, self.srcRank)

            # Create the regrid object
            ro = regrid2.gsRegrid.Regrid(self.srcGrid, self.dstGrid)

            # Set the mask
            if mask is not None:
                if len(mask.shape) > 3:
                    raise RegridError, \
                           'Ranks greater than three are not supported'
                self.mask = _setMaskDtype(mask)
                ro.setMask(self.mask)

            # Compute the weights
            self.nitermax = 20
            self.nlat = self.dstGrid[0].shape[0]
            self.tolpos = 0.01 * 180. / (self.nlat-1)
            if 'nitermax' in args.keys():
                self.nitermax = args['nitermax']
            if 'tolpos' in args.keys():
                self.tolpos = args['tolpos']
            ro.computeWeights(nitermax = self.nitermax, 
                              tolpos   = self.tolpos)
            self.regridTool = 'gsregrid'
            self.regridObj = ro
        else:
            raise RegridError, ''' Regrid tools are: regrid2,   
                  (gsRegrid or libcf), 
                  (esmf or esmp)'''
        
    def __call__(self, inData, **args):
        """
        Apply the interpolation.
        @param inData The input data
        @param missing Missing value
        @param order The order of the data
        @param mask Mask
        @param **args Optional keyword arguments for each regrid type
                regrid2 has: missing = None, order = None, mask = None, 
        """

        from cdms2.avariable import AbstractVariable
        from cdms2.tvariable import TransientVariable
        if isinstance(inData, AbstractVariable):
            if not isinstance(inData, numpy.ndarray):
                inData = inData.data

            attrs = copy.copy(inData.attributes)
            varid = inData.id
            axisList = list(map(lambda x: x[0].clone(), inData.getDomain()))
            inputIsVariable = 1
            if 'order' not in args.keys():
                order = inData.getOrder()
            #this expects contiguous arrays
            if isinstance(inData, TransientVariable) and inData.iscontiguous() is False:
                inData = inData.ascontiguous()
        else:
            inputIsVariable = 0
        
        missing = None
        if 'missing' in args.keys():
            missing = args['missing']

        if self.regridTool == 'regrid2':
            result = self.regridObj(inData, args)
        elif self.regridTool == 'esmp':
            if not self.esmfImported:
                string = "ESMP is not installed or is unable to be imported"
                raise RegridError, string

            if self.location is None:
                location = ESMP.ESMP_MESHLOC_NODE
            else:
                location = self.location

            # Make sure we are passing a ndarray
            self.srcField = esmf.EsmfStructField(self.srcMesh, inData.id, 
                                                 inData.data, 
                                                 meshloc = location)
            outData = numpy.zeros(self.dstMesh.getNodeDims(), inData.dtype)
            self.dstField = esmf.EsmfStructField(self.dstMesh, inData.id, 
                                                 outData, 
                                                 meshloc = location)
            if self.regridMethod is not None:
                method = self.regridMethod
            else:
                method = ESMP.ESMP_REGRIDMETHOD_BILINEAR
                
            if self.unMappedAction is not None:
                unMappedAction = self.unMappedAction
            else:
                unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
                
            self.regrid = esmf.EsmfRegrid(self.srcField, self.dstField, method,
                                          unMappedAction)
            # Call the regrid proceedure
            self.regrid()
            ptr = self.dstField.getPointer()
            outData.flat = self.dstField.getPointer()
            result = outData
            
            # Need to convert this to a cdms2 variable
                
        elif self.regridTool == 'scrip':
            pass  
        elif self.regridTool == 'gsregrid':

            # The data has a mask and the mask has not been set previously
            # If the mask is then set, the weights must be computed...
            if inData.mask.size > 1 and self.regridObj.maskSet is False:
                # Reset the weightsComputed flag since they will be recalculated
                self.regridObj.weightsComputed = False
                self.regridObj.setMask(inData.mask)
                # Recompute/Compute the weights taking the mask into account
                self.regridObj.computeWeights(tolpos = self.tolpos, 
                                              nitermax = self.nitermax)

            # If the weights have somehow escaped being generated, compute them
            if not self.regridObj.weightsComputed:
                self.regridObj.computeWeights(tolpos = self.tolpos, 
                                              nitermax = self.nitermax)

            # Create the result TransientVariable (if input inData is an AbstractVariable)
            # or masked array
            hasTime = None
            if inputIsVariable==1:
                hasTime = inData.getTime()
                if isinstance(self.dstGrid, list):
                    if len(self.dstGrid) == 2:
                        index = 0
                    else:
                        index = 1
                    from cdms2.coord import TransientAxis2D as T2D
                    lats = T2D(self.regridObj.dst_coords[index], id = 'lat')
                    lons = T2D(self.regridObj.dst_coords[index+1], id = 'lon')
                    grid = cdms2.hgrid.TransientCurveGrid(lats, lons, id = 'CurveGrid')
                
                shape = grid.shape
                axisList = list(grid.getAxisList())
                if inData.rank() == 2: 
                    pass
                elif inData.rank() == 3:
                    if inData.getTime() is not None:
                        axisList = [inData.getTime()] + list(axisList)
                        shape = tuple(list(axisList[0].shape) + list(shape))
                    else:
                        axisList = [inData.getLevel()] + list(axisList)
                        shape = tuple(list(axisList[0].shape) + list(shape))
                elif inData.rank() == 4 and inData.getTime() is not None:
                        aL = [inData.getTime(), inData.getLevel()]
                        axisList = aL + list(axisList)
                        s = []
                        for a in aL:
                            s.append(a.shape)
                        shape = tuple(s + list(shape))
                else:
                    raise RegridError, \
                          'Ranks > 4 currently not supported though this API'
                axisList = tuple(axisList)

            # Loop over the time variable
            nTime = 1
            if hasTime is not None:
                nTime = len(inData.getTime())
                outVar = numpy.zeros(shape, inData.dtype)
                for iTime in range(nTime):
                    outVar[iTime, ...] = self.regridObj(inData[iTime, ...])
            else:
                outVar = self.regridObj(inData)

            # Correct the shape of output weights
            amskout = numpy.ma.zeros(outVar.shape, numpy.bool8)
            if inputIsVariable == 1:
                if inData.fill_value is not None:
                    amskout = 1 - (outVar == inData.fill_value)

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


            if inputIsVariable:
                result = cdms2.createVariable(outVar, mask = outmask, 
                                              fill_value = inData.fill_value,
                                              axes = axisList, 
                                              grid = grid, 
                                              attributes = attrs, id = varid)
            else:
                result = numpy.ma.masked_array(outVar, mask = outmask, fill_value = missing)

        

        return result

    def __del__(self):
        if self.regridTool == 'esmp':
            pass

            # Why does this fail?
            # esmf.finalize()
         
