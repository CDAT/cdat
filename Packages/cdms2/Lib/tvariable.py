## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"""
TransientVariable (created by createVariable)
is a child of both AbstractVariable and the masked array class.
Contains also the write part of the old cu interface.
"""
import json
import re
import types
import typeconv
import numpy
from numpy import sctype2char
from error import CDMSError
from avariable import AbstractVariable

from axis import createAxis, AbstractAxis
from grid import createRectGrid, AbstractRectGrid
from hgrid import AbstractCurveGrid
from gengrid import AbstractGenericGrid

# dist array support 
HAVE_MPI = False
try:
    from mpi4py import MPI
    HAVE_MPI = True
except:
    pass


id_builtin = id                         # built_in gets clobbered by keyword

def fromJSON(jsn):
    """ Recreate a TV from a dumped jsn object"""
    D = json.loads(jsn)

    ## First recreates the axes
    axes=[]
    for a in D["_axes"]:
        ax = createAxis(numpy.array(a["_values"],dtype=a["_dtype"]),id=a["id"])
        for k,v in a.iteritems():
            if not k in ["_values","id","_dtype"]:
                setattr(ax,k,v)
        axes.append(ax)
    ## Now prep the variable
    V= createVariable(D["_values"],id=D["id"],typecode=D["_dtype"])
    V.setAxisList(axes)
    for k,v in D.iteritems():
        if not k in ["id","_values","_axes","_grid","_fill_value","_dtype",]:
            setattr(V,k,v)
    V.set_fill_value(D["_fill_value"])
    return V


class TransientVariable(AbstractVariable,numpy.ma.MaskedArray):
    "An in-memory variable."
    variable_count = 0
    missing_value = numpy.ma.MaskedArray.fill_value

    def _getShape(self):
        return self._data.shape

    shape = property(_getShape,None)
    def iscontiguous(self):
        return self.flags['CONTIGUOUS']

    def ascontiguousarray(self):
        d = numpy.ma.getdata(self)
        out = numpy.ascontiguousarray(d)
        m = numpy.ma.getmask(self)
        if m is not numpy.ma.nomask:
            m= numpy.ascontiguousarray(m)
        out = TransientVariable(out,mask=m,attributes=self.attributes)
        out.setAxisList(self.getAxisList())
        return out
    
    ascontiguous = ascontiguousarray
    
    def asma(self):
        return numpy.ma.array(self._data,mask=self._mask)
    
    def _update_from(self,obj):
        numpy.ma.MaskedArray._update_from(self,obj)
        if not hasattr(self,'___cdms_internals__'):
            self.__dict__['___cdms_internals__']=['__cdms_internals__','___cdms_internals__','_node_','parent','attributes','shape']
        if not hasattr(self,'attributes'):
            self.attributes={}
        self._grid_ = getattr(obj,'_grid_',None)
        try:
            for nm,val in obj.__dict__.items():
                if nm[0]=='_':
##                     print nm
                    pass
##                     self.__dict__[nm]=val
                else:
                    setattr(self,nm,val)
        except Exception,err:
            pass
        id = getattr(self,'id',None)
        if id is None:
            TransientVariable.variable_count+=1
            id = 'variable_'+str(TransientVariable.variable_count)
            self.id=id
        self.name = getattr(obj,'name',id)
        if not hasattr(self,'__domain'):
            self.initDomain(axes=None)


    def __array_finalize__(self,obj):
        numpy.ma.MaskedArray.__array_finalize__(self,obj)
        return

    
    __mul__    = AbstractVariable.__mul__
    __rmul__   = AbstractVariable.__rmul__
    __imul__   = AbstractVariable.__imul__
    __abs__    = AbstractVariable.__abs__
    __neg__    = AbstractVariable.__neg__
    __add__    = AbstractVariable.__add__
    __iadd__   = AbstractVariable.__iadd__
    __radd__   = AbstractVariable.__radd__
    __lshift__ = AbstractVariable.__lshift__
    __rshift__ = AbstractVariable.__rshift__
    __sub__    = AbstractVariable.__sub__
    __rsub__   = AbstractVariable.__rsub__    
    __isub__   = AbstractVariable.__isub__    
    __div__    = AbstractVariable.__div__
    __rdiv__   = AbstractVariable.__rdiv__
    __idiv__   = AbstractVariable.__idiv__
    __pow__   = AbstractVariable.__pow__
    __eq__    = AbstractVariable.__eq__
    __ne__    = AbstractVariable.__ne__
    __lt__    = AbstractVariable.__lt__
    __le__    = AbstractVariable.__le__
    __gt__    = AbstractVariable.__gt__
    __ge__    = AbstractVariable.__ge__
    __sqrt__    = AbstractVariable.__sqrt__

    def __init__(self,data, typecode=None, copy=1, savespace=0, 
                 mask=numpy.ma.nomask, fill_value=None, grid=None,
                 axes=None, attributes=None, id=None, copyaxes=1, dtype=None, 
                 order=False, no_update_from=False,**kargs):
        """createVariable (self, data, typecode=None, copy=0, savespace=0, 
                 mask=None, fill_value=None, grid=None,
                 axes=None, attributes=None, id=None, dtype=None, order=False)
           The savespace argument is ignored, for backward compatibility only.
        """

        # tile index, None means no mosaic 
        self.tileIndex = None
        
        # Compatibility: assuming old typecode, map to new
        if dtype is None and typecode is not None:
            dtype = typeconv.convtypecode2(typecode)
        typecode = sctype2char(dtype)
        if type(data) is types.TupleType:
            data = list(data)
        
        AbstractVariable.__init__ (self)

        if isinstance(data, AbstractVariable):
            if not isinstance(data, TransientVariable):
                data = data.subSlice()
##             if attributes is None: attributes = data.attributes
            if axes is None and not no_update_from:
                axes = map(lambda x: x[0], data.getDomain())
            if grid is None and not no_update_from:
                grid = data.getGrid()
                if (grid is not None) and (not isinstance(grid, AbstractRectGrid)) \
                                      and (not grid.checkAxes(axes)):
                    grid = grid.reconcile(axes) # Make sure grid and axes are consistent

        ncopy = (copy!=0)


        # Initialize the geometry
        if grid is not None:
            copyaxes=0                  # Otherwise grid axes won't match domain.
        if axes is not None:
            self.initDomain(axes, copyaxes=copyaxes)           # Note: clobbers the grid, so set the grid after.
        if grid is not None:
            self.setGrid(grid)
 
        # Initialize attributes
        fv = self.fill_value
        if attributes is not None:
            for key, value in attributes.items():
                if (key in ['shape','flat','imaginary','real'] or key[0]=='_') and key not in ['_FillValue']:
                    raise CDMSError, 'Bad key in attributes: ' + key
                elif key == 'missing_value':
                    #ignore if fill value given explicitly
                    if fill_value is None:
                        fv = value
                elif key not in ['scale_factor','add_offset']:
                    setattr(self, key, value)

        # Sync up missing_value attribute and the fill value.
        self.missing_value = fv
        if id is not None:
            if not isinstance(id,(unicode,str)): 
                raise CDMSError, 'id must be a string'
            self.id = id
        elif hasattr(data,'id'):
            self.id = data.id

        if self.id is None:
            TransientVariable.variable_count = TransientVariable.variable_count + 1
            self.id = 'variable_' + str(TransientVariable.variable_count)
        self.name = getattr(self, 'name', self.id)

        # MPI data members
        self.__mpiComm = None
        if HAVE_MPI:
            self.__mpiComm = MPI.COMM_WORLD
        self.__mpiWindows = {}
        self.__mpiType = self.__getMPIType()


    def _getmissing(self):
        return self._missing
    def _setmissing(self,value):
        self._missing=numpy.array(value).astype(self.dtype)

    missing = property(_getmissing,_setmissing)
    def __new__(cls, data, typecode=None, copy=0, savespace=0, 
                 mask=numpy.ma.nomask, fill_value=None, grid=None,
                 axes=None, attributes=None, id=None, copyaxes=1, dtype=None, order=False,**kargs):
        """createVariable (self, data, typecode=None, copy=0, savespace=0, 
                 mask=None, fill_value=None, grid=None,
                 axes=None, attributes=None, id=None, dtype=None, order=False)
           The savespace argument is ignored, for backward compatibility only.
        """
        # Compatibility: assuming old typecode, map to new
        if dtype is None and typecode is not None:
            dtype = typeconv.convtypecode2(typecode)
        typecode = sctype2char(dtype)
        if type(data) is types.TupleType:
            data = list(data)
        if isinstance(data, AbstractVariable):
            if not isinstance(data, TransientVariable):
                data = data.subSlice()
        if isinstance(data, numpy.ma.MaskedArray):
            try:
                if fill_value is None: fill_value = data.fill_value
            except:
                pass

        ncopy = (copy!=0)
        if mask is None:
            try:
                mask = data.mask
            except Exception,err:
                mask = numpy.ma.nomask

        # Handle the case where ar[i:j] returns a single masked value
        if data is numpy.ma.masked:
            #shape = tuple(len(axes)*[1])
            data = numpy.ma.masked.data
            #data.shape = shape
            mask = numpy.ma.masked.mask
            #mask.shape = shape
##         if data.getattr('mask',None) is not numpy.ma.nomask:
##             mask = data.mask
##         print 'passing:',mask.shape,data.shape,numpy.shape(cls)
        self = numpy.ma.MaskedArray.__new__(cls, data, dtype = dtype,
                                      copy = ncopy,
                                      mask = mask,
                                      fill_value = numpy.array(fill_value).astype(dtype),
                                      subok = False,
                                      order = order)

            
        
        return self

    # typecode = numpy.ma.array.typecode
    def typecode(self):
        return self.dtype.char

    def assignValue(self,data):
        self[...] = data

    def getValue(self, squeeze=1):
        return self.filled()

    def expertSlice (self, slicelist):
        return numpy.ma.MaskedArray.__getitem__(self, slicelist)

    def initDomain (self, axes, copyaxes=1):
        # lazy evaluation via getAxis to avoid creating axes that aren't ever used.
        newgrid = None
        self.__domain = [None]*self.rank()
        if axes is not None:
            flataxes = []
            try:
                iter(axes)
            except TypeError:
                axes = (axes,)
            for item in axes:
                if isinstance(item, AbstractAxis) or item is None:
                    flataxes.append(item)
                elif isinstance(item, AbstractRectGrid) or isinstance(item, AbstractCurveGrid):
                    flataxes.append(item.getAxis(0))
                    flataxes.append(item.getAxis(1))
                    copyaxes=0
                    newgrid = item
                elif isinstance(item, AbstractGenericGrid):
                    flataxes.append(item.getAxis(0))
                    copyaxes=0
                    newgrid = item
                else:
                    raise CDMSError, "Invalid item in axis list:\n"+`item`
            if len(flataxes) != self.rank():
                raise CDMSError, "Wrong number of axes to initialize domain."
            for i in range(len(flataxes)):
                if flataxes[i] is not None:
                    if (not flataxes[i].isVirtual()) and copyaxes==1:
                        self.copyAxis(i, flataxes[i])
                    else:
                        self.setAxis(i, flataxes[i]) # No sense copying a virtual axis.
            if newgrid is not None:     # Do this after setting the axes, so the grid is consistent
                self.setGrid(newgrid)

    def getDomain(self):
        for i in range(self.rank()):
            if self.__domain[i] is None:
                junk = self.getAxis(i)  # will force a fill in
        return self.__domain

    def getAxis (self, n):
        if n < 0: n = n + self.rank()
        if self.__domain[n] is None:
            length = numpy.ma.size(self, n)
            # axis = createAxis(numpy.ma.arange(numpy.ma.size(self, n), typecode=numpy.Float))
            axis = createAxis(numpy.ma.arange(numpy.ma.size(self, n), dtype=numpy.float_))
            axis.id = "axis_" + str(n)
            self.__domain[n] = (axis, 0, length, length)
        return self.__domain[n][0]
        
    def setAxis (self, n, axis, savegrid=0):
        """Set n axis of self to a copy of axis. (0-based index)
        """
        if n < 0: n = n + self.rank()
        axislen = self.shape[n]
        if len(axis)!=axislen:
            raise CDMSError,"axis length %d does not match corresponding dimension %d"%(len(axis),axislen)
        if not isinstance(axis, AbstractAxis):
            raise CDMSError,"copydimension, other not a slab."
        self.__domain[n] = (axis, 0, len(axis), len(axis))

    def setAxisList(self, axislist):
        """Set the axes to axislist."""
        for i in range(len(axislist)):
            self.setAxis(i, axislist[i])

    def copyAxis (self, n, axis):
        """Set n axis of self to a copy of axis. (0-based index)
           Invalidates grid.
        """
        if n < 0: n = n + self.rank()
        if not isinstance(axis, AbstractAxis):
            raise CDMSError,"copydimension, other not an axis."
        b = axis.getBounds()
        mycopy = createAxis(axis[:], b)
        mycopy.id = axis.id
        for k, v in axis.attributes.items():
           setattr(mycopy, k, v)
        self.setAxis (n, mycopy)
   
    def copyDomain (self, other):
        "Set the axes and grid by copying variable other."
        if not isinstance(other, AbstractVariable):
            raise CDMSError,"copyDomain, other not a variable."
        if self.rank() != other.rank():
            raise CDMSError, "copyDomain, ranks do not match."
        for i in range(self.rank()):
            self.copyAxis(i, other.getAxis(i))
        self.setGrid(other.getGrid())

    def getGrid(self):
        if self._grid_ is None:
            order = ''
            for i in range(self.rank()):
                ax = self.getAxis(i)
                if ax.isLatitude():
                    order = order+'y'
                    lat = ax
                elif ax.isLongitude():
                    order = order+'x'
                    lon = ax
                if len(order)==2: break

            if order in ['yx','xy']:
                self._grid_ = createRectGrid(lat,lon,order)
        return self._grid_

    def astype (self, tc):
        "return self as array of given type."
        maresult = numpy.ma.MaskedArray.astype(self,tc)
        return TransientVariable(maresult, copy=0, axes=self.getAxisList(),
                                 attributes=self.attributes, id=self.id, grid=self.getGrid())

    def setMaskFromGridMask(self, mask, gridindices):
        """Set the mask for self, given a grid mask and the variable domain
        indices corresponding to the grid dimensions.
        """

        # Get the variable indices that are NOT in gridindices
        tprep = []
        shapeprep = []
        for i in range(self.rank()):
            if i not in gridindices:
                tprep.append(i)
                shapeprep.append(self.shape[i])

        # Broadcast mask
        if tprep!=[]:
            newshape = tuple(shapeprep + list(mask.shape))
            bigmask = numpy.resize(mask, newshape)

            # Generate the tranpose vector
            t = tuple(tprep + list(gridindices))
            tinv = [0]*len(t)
            for i in range(len(t)):
                tinv[t[i]] = i

            # And reshape to fit the variable
            if tinv!=range(len(tinv)):
                bigmask = numpy.transpose(bigmask, tuple(tinv))

        else:
            bigmask = mask

        # Apply the mask to self
        currentmask = self.mask
        if currentmask is not numpy.ma.nomask:
            bigmask = numpy.logical_or(currentmask, bigmask)

        result = TransientVariable(self, mask=bigmask)
        return result

# Old cu interface
    def copydimension (self, idim, other, jdim):
        """Set idim dimension of self to variable other's jdim'th 
           This is for old cu compatibility. Use copyAxis for new code.
        """
        if not isinstance(other, AbstractVariable):
            raise CDMSError,"copydimension, other not a variable."
        a = other.getAxis(jdim)
        self.copyAxis(idim, a)

    def setdimattribute(self, dim, field, value):
        "Set the attribute named field from the dim'th dimension."
        if dim < 0 or dim >= self.rank():
            raise CDMSError, "setdimattribute, dim out of bounds."
        d = self.getAxis(dim)
        if field == "name":
            if not type(value) == types.StringType:
               raise CDMSError, "setdimattribute: name not a string"
            d.id = value
            
        elif field == "values":
            # note -- invalidates grid, may break old code.
            a = createAxis(numpy.ma.filled(value[:]))
            if hasattr(d, 'units'):
                a.units = d.units
            a.id = d.id
            self.setAxis(dim, a)

        elif field == "units":
            if not type(value) == types.StringType:
               raise CDMSError, "setdimattribute: units not a string"
            d.units = value

        elif field == "weights":
            # Well, you can't really do this without modifying the grid
            raise CDMSError, "setdimattribute weights not implemented."

        elif field == "bounds":
            if value is None:
               d.setBounds(None)
            else:
               b = numpy.ma.filled(value)
               if numpy.ma.rank(b) == 2:
                   d.setBounds(b)
               elif numpy.ma.rank(b) == 1:
                   b1 = numpy.zeros((len(b)-1,2), b.dtype.char)
                   b1[:,0] = b[:-1]
                   b1[:,1] = b[1:]
                   d.setBounds(b1)
               else:
                   raise CDMSError, \
                   "setdimattribute, bounds improper shape: " + b.shape
        else:
            setattr(d, field, value)

    def clone(self, copyData=1):
        """clone (self, copyData=1)
        Return a copy of self as a transient variable.
        If copyData is 1 (default), make a separate copy of the data."""
        result = createVariable(self, copy=copyData)
        return result

    def dumps(self,*args,**kargs):
        ## Probably need something for curv/gen grids
        """ Dumps Variable to a jason object, args are passed directly to json.dump"""
        J={}
        for k,v in self.attributes.iteritems():
            if k=="autoApiInfo":
                continue
            J[k]=v
        J['id']=self.id
        axes=[]
        for a in self.getAxisList():
            ax={}
            for A,v in a.attributes.iteritems():
                ax[A]=v
            ax['id']=a.id
            ax["_values"]=a[:].tolist()
            ax["_dtype"]=a[:].dtype.char
            axes.append(ax)
        J["_axes"]=axes
        J["_values"]=self[:].filled(self.fill_value).tolist()
        J["_fill_value"]=float(self.fill_value)
        J["_dtype"]=self.typecode()
        J["_grid"]=None #self.getGrid()
        return json.dumps(J,*args,**kargs)

    def isEncoded(self):
        "Transient variables are not encoded"
        return 0

    def __len__ (self):
        "Length of first dimension"
        if self.rank()>0:
            (axis,start,length,true_length) = self.getDomain()[0]
        else:
            length = 0
        return length

    def __str__ (self):
        return numpy.ma.MaskedArray.__str__(self)

    def __repr__ (self):
        return self.id + '\n' + numpy.ma.MaskedArray.__repr__(self) + '\n'

    def set_fill_value(self, value):
        "Set missing value attribute and fill value"
        AbstractVariable.setMissing(self, value)
        #self.__dict__['_fill_value'] = self.missing_value
        ## Fix submitted by Ghislain Picard, this was broken with numpy 1.5
        numpy.ma.MaskedArray.set_fill_value(self,value)

    def setMissing (self, value):
        "Set missing value attribute and fill value"
        self.set_fill_value(value)

    # For aggregation server interface. Use clone to make a true copy.
    def copy(self):
        return self.filled()

    def setTileIndex(self, index):
        """
        Set the tile index (for mosaics)
        index: tile index
        """
        self.tileIndex = index

    def getTileIndex(self):
        """
        Get the tile index (for mosaics)
        """
        return self.tileIndex

    def toVisit(self, filename, format='Vs', sphereRadius=1.0, 
                maxElev=0.1):
        """
        Save data to file for postprocessing by the VisIt visualization tool
        filename: name of the file where the data will be saved
        format: 'Vs' for VizSchema, 'VTK' for VTK, ...
        sphereRadius: radius of the earth
        maxElev: maximum elevation for representation on the sphere
        """
        import mvSphereMesh
        import mvVTKSGWriter
        import mvVsWriter
        try:
            # required by mvVsWriter
            import tables
        except:
            # fall back 
            format = 'VTK'

        def generateTimeFileName(filename, tIndex, tIndexMax, suffix):
            ndigits = len('%d'%tIndexMax)
            itdigits = len('%d'%tIndex)
            tiStr = '0'*(ndigits-itdigits) + ('%d'%tIndex)
            return re.sub(r'\.' + suffix, '_%s.%s' % (tiStr, suffix), 
                          filename)

        # determine whether data are time dependent
        timeAxis = self.getTime()

        # if time dependent, then which index is time?
        timeIndex = -1
        if timeAxis:
            counter = -1
            for axis in self.getAxisIds():
                counter += 1
                if axis == 'time':
                    timeIndex = counter
        
        if timeAxis == None or timeIndex == -1:
            # static data
            if format == 'VTK':
                vw = mvVTKSGWriter.VTKSGWriter(self, maxElev)
                if filename.find('.vtk') == -1: 
                    filename += '.vtk'
                vw.write(filename)
            else:
                vw = mvVsWriter.VsWriter(self, maxElev)
                if filename.find('.vsh5') == -1: 
                    filename += '.vsh5'
                vw.write(filename)
        else:
            # time dependent data
            tIndexMax = len(timeAxis)
            for tIndex in range(tIndexMax):
                sliceOp = 'self[' + (':,'*timeIndex) + ('%d,'%tIndex) + '...]'
                var = eval(sliceOp)
                if format == 'VTK':
                    if filename.find('.vtk') == -1:
                        filename += '.vtk'
                    tFilename = generateTimeFileName(filename, 
                                                     tIndex, tIndexMax, 'vtk')
                    vw = mvVTKSGWriter.VTKSGWriter(var, maxElev)
                    vw.write(tFilename)
                else:
                    if filename.find('.h5') == -1:
                        filename += '.h5'
                    tFilename = generateTimeFileName(filename, 
                                                     tIndex, tIndexMax, 'h5')
                    vw = mvVsWriter.VsWriter(var, maxElev)
                    vw.write(tFilename)
       
    # Following are distributed array methods, they require mpi4py 
    # to be installed

    def setMPIComm(self, comm):
        """
        Set the MPI communicator. This is a no-op if MPI 
        is not available.
        """
        if HAVE_MPI:
            self.__mpiComm = comm

    def getMPIRank(self):
        """
        Return the MPI rank
        """
        if HAVE_MPI:
            return self.__mpiComm.Get_rank()
        else:
            return 0

    def getMPISize(self):
        """
        Return the MPI communicator size
        """
        if HAVE_MPI:
            return self.__mpiComm.Get_size()
        else:
            return 1

    def exposeHalo(self, ghostWidth=1):
        """
        Expose the halo to other processors. The halo is the region
        within the local MPI data domain that is accessible to other 
        processors. The halo encompasses the edge of the data region
        and has thickness ghostWidth.

        ghostWidth - width of the halo region (> 0)
        """
        if HAVE_MPI:
            shape = self.shape
            ndims = len(shape)
            for dim in range(ndims):
                for drect in (-1, 1):
                    # the window id uniquely specifies the
                    # location of the window. We use 0's to indicate
                    # a slab extending over the entire length for a
                    # given direction, a 1 represents a layer of
                    # thickness ghostWidth on the high index side,
                    # -1 on the low index side.
                    winId = tuple( [0 for i in range(dim) ] \
                                   + [drect] + \
                                   [0 for i in range(dim+1, ndims) ] )

                    slce = slice(0, ghostWidth)
                    if drect == 1:
                        slce = slice(shape[dim] - ghostWidth, shape[dim])

                    slab = self.__getSlab(dim, slce)

                    # create the MPI window
                    dataSrc = numpy.zeros(self[slab].shape, self.dtype) 
                    dataDst = numpy.zeros(self[slab].shape, self.dtype) 
                    self.__mpiWindows[winId] = {
                        'slab': slab,
                        'dataSrc': dataSrc,
                        'dataDst': dataDst,
                        'window': MPI.Win.Create(dataSrc, comm=self.__mpiComm),
                        }
                
    def getHaloEllipsis(self, side):
        """
        Get the ellipsis for a given halo side. 
        
        side - a tuple of zeros and one +1 or -1.  To access
               the "north" side for instance, set side=(1, 0),
               (-1, 0) to access the south side, (0, 1) the east
               side, etc. This does not involve any communication.

        Return none if halo was not exposed (see exposeHalo)
        """
        if HAVE_MPI and self.__mpiWindows.has_key(side):
            return self.__mpiWindows[side]['slab']
        else:
            return None

    def fetchHaloData(self, pe, side):
        """
        Fetch the halo data from another processor. The halo side
        is a subdomain of the halo that is exposed to other 
        processors. It is an error to call this method when
        MPI is not enabled. This is a collective method (must
        be called by all processes), which involves synchronization
        of data among all processors.

        pe       -  processor owning the halo data. This is a no
                    operation when pe is None. 
        side     -  a tuple of zeros and one +1 or -1.  To access
                    the "north" side for instance, set side=(1, 0),
                    (-1, 0) to access the south side, (0, 1) the east
                    side, etc. 

        Note: collective, all procs must invoke this method. If some 
        processors should not fetch then pass None for pe.
        """
        if HAVE_MPI:
            iw = self.__mpiWindows[side]
            slab = iw['slab']
            dataSrc = iw['dataSrc']
            dataDst = iw['dataDst']

            # copy src data into buffer
            dataSrc[...] = self[slab]

            win = iw['window']
            win.Fence() # get the data ready
            if pe is not None:
                win.Get( [dataDst, self.__mpiType], pe )
            win.Fence() # make sure the communication completed
            return dataDst
        else:
            raise CDMSError, 'Must have MPI to invoke fetchHaloData'

    def freeHalo(self):
        """
        Free the MPI windows attached to the halo. This must be 
        called before MPI_Finalize.
        """
        for iw in self.__mpiWindows:
            self.__mpiWindows[iw]['window'].Free()        

    def __getSlab(self, dim, slce):
        """
        Get slab. A slab is a multi-dimensional slice extending in
        all directions except along dim where slce applies
        
        dim      - dimension (0=first index, 1=2nd index...)
        slce     - python slice object along dimension dim
        
        return slab
        """
        ndims = len(self.shape)
        
        slab = [ slice(0, None) for i in range(dim) ] \
                    + [slce] + \
                  [ slice(0, None) for i in range(dim+1, ndims) ]
        return tuple(slab)

    def __getMPIType(self):
        """
        Return the MPI type of the array, or None
        if no match
        """
        typ = None
        dtyp = self.dtype
        if HAVE_MPI:
            if dtyp == numpy.float64:
                typ = MPI.DOUBLE
            elif dtyp == numpy.float32:
                typ = MPI.FLOAT
            elif dtyp == numpy.int64:
                typ = MPI.INT64_T
            elif dtyp == numpy.int32:
                typ = MPI.INT32_T
            elif dtyp == numpy.int16:
                typ = MPI.INT16_T
            elif dtyp == numpy.int8:
                typ = MPI.INT8_T
            else:
                return None          
        else:
            return typ

## PropertiedClasses.set_property(TransientVariable, 'shape', 
##                                nowrite=1, nodelete=1)

def createVariable(*args,**kargs):
    if kargs.get("fromJSON",False):
        return fromJSON(*args)
    else:
        return TransientVariable(*args,**kargs)

def isVariable (s):
    "Is s a variable?"
    return isinstance(s, AbstractVariable)

def asVariable(s, writeable=1):
    """Returns s if s is a Variable; if writeable is 1, return 
       s if s is a TransientVariable. If s is not a variable of 
       the desired type, attempt to make it so and return that.
       If we fail raise CDMSError
    """
    target_class = AbstractVariable
    if writeable: target_class = TransientVariable
    if isinstance(s, target_class):
        return s
    elif isinstance(s, AbstractVariable):
        return s.subSlice()
    
    try:
        result = createVariable(s)
    except CDMSError:
        result =  None
    
    # if result.dtype.char == numpy.ma.PyObject:
    if issubclass(result.dtype.type, numpy.object_):
        result = None
    if result is None:
        raise CDMSError, "asVariable could not make a Variable from the input."
    return result

if __name__ == '__main__':
    for s in [(20,), (4,5)]:
        x = numpy.arange(20)
        x.shape = s
        t = createVariable(x)
        assert t.shape == s
        assert t.missing_value == t._fill_value
        assert numpy.ma.allclose(x, t)
        assert t.dtype.char == numpy.int
        assert numpy.ma.size(t) == numpy.ma.size(x)
        assert numpy.ma.size(t,0) == len(t)
        assert numpy.ma.allclose(t.getAxis(0)[:], numpy.ma.arange(numpy.ma.size(t,0)))
        t.missing_value = -99
        assert t.missing_value == -99
        assert t.fill_value == -99
    t = createVariable(numpy.ma.arange(5), mask=[0,0,0,1,0])
    t.set_fill_value (1000)
    assert t.fill_value == 1000
    assert t.missing_value == 1000
    t.missing_value = -99
    assert t[2] == 2
    t[3] = numpy.ma.masked
    assert t[3] is numpy.ma.masked
    f = createVariable(numpy.ma.arange(5, typecode=numpy.float32), mask=[0,0,0,1,0])
    f2 = createVariable(numpy.ma.arange(5, typecode=numpy.float32), mask=[0,0,0,1,0])
    f[3] = numpy.ma.masked
    assert f[3] is numpy.ma.masked
    assert numpy.ma.allclose(2.0, f[2])
    t.setdimattribute(0, 'units', 'cm')
    assert t.getdimattribute(0, 'units') == 'cm'
    t.setdimattribute(0, 'name', 'fudge')
    assert t.getdimattribute(0, 'name') == 'fudge'
    f2b = f2.getdimattribute(0, 'bounds')
    t.setdimattribute(0, 'bounds', f2b)
    assert numpy.ma.allclose(f.getdimattribute(0,'bounds'), f2.getdimattribute(0,'bounds'))
    print "Transient Variable test passed ok."

