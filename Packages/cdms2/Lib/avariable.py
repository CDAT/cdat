## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by
## Further modified to be pure new numpy June 24th 2008

"CDMS Variable objects, abstract interface"
import numpy
import types, string, re
import cdmsNode
from cdmsobj import CdmsObj
import cdms2
from slabinterface import Slab
from sliceut import *
from error import CDMSError
from axis import axisMatchIndex, axisMatchAxis, axisMatches, unspecified, CdtimeTypes, AbstractAxis
import selectors
import copy
# from regrid2 import Regridder, PressureRegridder, CrossSectionRegridder
#import PropertiedClasses
from convention import CF1
from grid import AbstractRectGrid
#import internattr

InvalidRegion = "Invalid region: "
OutOfRange = "Coordinate interval is out of range or intersection has no data: "
NotImplemented = "Child of AbstractVariable failed to implement: "

_numeric_compatibility = False          # Backward compatibility with numpy behavior
                                        # False: return scalars from 0-D slices
                                        #        MV axis=None by default
                                        # True:  return 0-D arrays
                                        #        MV axis=1 by default

def setNumericCompatibility(mode):
    global _numeric_compatibility
    if mode==True or mode=='on':
        _numeric_compatibility = True
    elif mode==False or mode=='off':
        _numeric_compatibility = False

def getNumericCompatibility():
    return _numeric_compatibility

class AbstractVariable(CdmsObj, Slab):
    def __init__ (self, parent=None, variableNode=None):
        """Not to be called by users.
           variableNode is the variable tree node, if any.
           parent is the containing dataset instance.
        """
        if variableNode is not None and variableNode.tag !='variable':
            raise CDMSError, 'Node is not a variable node'
        CdmsObj.__init__(self, variableNode)
        val = self.__cdms_internals__ + ['id','domain',"autoApiInfo"]
        self.___cdms_internals__ = val 
        Slab.__init__(self)
        self.id = None                  # Transient variables key on this to create a default ID
        self.parent = parent
        self._grid_ = None      # Variable grid, if any
        if not hasattr(self,'missing_value'):
            self.missing_value = None
        # Reminder: children to define self.shape and set self.id

    def __array__ (self, t=None, context=None):  #Numeric, ufuncs call this
        return numpy.ma.filled(self.getValue(squeeze=0))

    def __call__ (self, *args,  **kwargs):
        "Selection of a subregion using selectors"
        # separate options from selector specs
        d = kwargs.copy()
        raw = d.setdefault('raw', 0)
        squeeze = d.setdefault('squeeze', 0)
        grid = d.setdefault('grid', None)
        order = d.setdefault('order', None)
        del d['squeeze'], d['grid'], d['order'], d['raw']
        # make the selector
        s = selectors.Selector(*args, **d)
        # get the selection
        return s.unmodified_select(self, raw=raw,
                                         squeeze=squeeze, 
                                         order=order, 
                                         grid=grid)

    select = __call__

    def rank (self):
        return len(self.shape)

    def _returnArray(self, ar, squeeze, singles=None):
        # ar is a Numeric array, numpy.ma, or scalar, possibly numpy.ma.masked.
        # job is to make sure we return an numpy.ma or a scalar.
        # If singles is not None, squeeze dimension indices in singles
        inf = 1.8e308
        if isinstance(ar,cdms2.tvariable.TransientVariable):
            result = numpy.ma.array(ar._data,mask=ar.mask)
        elif numpy.ma.isMaskedArray(ar):   #already numpy.ma, only need squeeze.
            result = ar
        elif isinstance(ar, numpy.ndarray):
            missing = self.getMissing()
            if missing is None:
                result = numpy.ma.masked_array(ar)
            elif missing==inf or missing!=missing: # (x!=x) ==> x is NaN
                result = numpy.ma.masked_object(ar, missing, copy=0)
            elif ar.dtype.char=='c':
                # umath.equal is not implemented
                resultmask = (ar==missing)
                if not resultmask.any():
                    resultmask = numpy.ma.nomask
                result = numpy.ma.masked_array(ar, mask=resultmask, fill_value=missing)
            else:
                result = numpy.ma.masked_values(ar, missing, copy=0)
        elif ar is numpy.ma.masked:
            return ar  
        else: # scalar, but it might be the missing value
            missing = self.getMissing()
            if missing is None:
                return ar #scalar
            else:
                result = numpy.ma.masked_values(ar, missing, copy=0)

        squoze = 0
        if squeeze:
            n = 1
            newshape=[]
            for s in result.shape:
               if s == 1: 
                   squoze = 1
                   continue
               else:
                   n = n * s
                   newshape.append(s)
        elif singles is not None:
            n = 1
            newshape=[]
            oldshape = result.shape
            for i in range(len(oldshape)):
               if i in singles: 
                   squoze = 1
                   continue
               else:
                   s = oldshape[i]
                   n = n * s
                   newshape.append(s)
            
        else:
            n = numpy.ma.size(result)
        if n == 1 and squeeze:
            return numpy.ma.ravel(result)[0] # scalar or masked
        if squoze:
            result.shape = newshape
        return result

    def generateGridkey(self, convention, vardict):
        """ generateGridkey(): Determine if the variable is gridded, and
        generate ((latname, lonname, order, maskname, class), lat, lon) if
        gridded, or (None, None, None) if not gridded. vardict is the variable
        dictionary of the parent """
        lat, nlat = convention.getVarLatId(self, vardict)
        lon, nlon = convention.getVarLonId(self, vardict)
        if (lat is not None) and (lat is lon):
            raise CDMSError, "Axis %s is both a latitude and longitude axis! Check standard_name and/or axis attributes."%lat.id

        # Check for 2D grid
        if (lat is None) or (lon is None):
            return None, lat, lon

        # Check for a rectilinear grid
        if isinstance(lat, AbstractAxis) and isinstance(lon, AbstractAxis) and (lat.rank()==lon.rank()==1):
            return self.generateRectGridkey(lat, lon), lat, lon

        # Check for a curvilinear grid:
        if lat.rank()==lon.rank()==2:

            # check that they are defined on the same indices as self
            vardomain = self.getAxisIds()
            allok = 1
            for axisid in lat.getAxisIds():
                if axisid not in vardomain:
                    allok = 0
                    break
            if allok:
                for axisid in lon.getAxisIds():
                    if axisid not in vardomain:
                        allok = 0
                        break
            
            # It's a curvilinear grid
            if allok:
                if hasattr(lat, 'maskid'):
                    maskid = lat.maskid
                else:
                    maskid = ''
                return (lat.id, lon.id, 'yx', maskid, 'curveGrid'), lat, lon

        # Check for a generic grid:
        if lat.rank()==lon.rank()==1:

            # check that they are defined on the same indices as self
            vardomain = self.getAxisIds()
            allok = 1
            for axisid in lat.getAxisIds():
                if axisid not in vardomain:
                    allok = 0
                    break
            if allok:
                for axisid in lon.getAxisIds():
                    if axisid not in vardomain:
                        allok = 0
                        break
            
            # It's a generic grid
            if allok:
                if hasattr(lat, 'maskid'):
                    maskid = lat.maskid
                else:
                    maskid = ''
                return (lat.id, lon.id, 'yx', maskid, 'genericGrid'), lat, lon

        return None, lat, lon

    def generateRectGridkey(self, lat, lon):
        """ generateRectGridkey(): Determine if the variable is gridded, rectilinear,
            and generate (latname, lonname, order, maskname, class) if gridded,
            or None if not gridded"""

        ilat = ilon = -1
        k = 0
        for axis in self.getAxisList():
            if axis is lon:
                ilon = k
            elif axis is lat:
                ilat = k
            k += 1

        if ilat==-1:
            raise CDMSError, "Cannot find latitude axis; check standard_name and/or axis attributes"
        if ilon==-1:
            raise CDMSError, "Cannot find longitude axis; check standard_name and/or axis attributes"

        if ilat<ilon:
            order = "yx"
        else:
            order = "xy"
        gridkey = (lat.id, lon.id, order, '', 'rectGrid')

        return gridkey

    def isAbstractCoordinate(self):
        return 0

    # Set the variable grid
    def setGrid(self, grid):
        if grid is None:
            gridok = 1
        else:
            alist = [d[0] for d in self.getDomain()]
            gridok = grid.checkAxes(alist)
        if not gridok:
            raise CDMSError, "grid does not match axes for variable %s"%self.id
        self._grid_ = grid

    def getDomain (self):
        "Get the list of axes"
        raise CDMSError, "getDomain not overriden in child"

    def getConvention(self):
        "Get the metadata convention associated with this object."
        if hasattr(self, 'parent') and self.parent is not None:
            result = self.parent._convention_
        else:
            result = CF1
        return result
            
# A child class may want to override this
    def getAxis(self, n):
        "Get the n-th axis"
        if n < 0: n = n + self.rank()
        return self.getDomain()[n][0]

    def getAxisIndex (self, axis_spec):
        """Return the index of the axis specificed by axis_spec.
         Argument axis_spec and be as for axisMatches
         Return -1 if no match.
        """
        for i in range(self.rank()):
            if axisMatches(self.getAxis(i), axis_spec):
                return i
        return -1

    def getAxisListIndex (self, axes=None, omit=None, order=None):
        """Return a list of indices of axis objects;
           If axes is not None, include only certain axes.
           less the ones specified in omit. If axes is None,
           use all axes of this variable.
           Other specificiations are as for axisMatchIndex.
        """
        return axisMatchIndex(self.getAxisList(), axes, omit, order)

    def getAxisList(self, axes = None, omit=None, order=None):
        """Get the list of axis objects; 
           If axes is not None, include only certain axes.
           If omit is not None, omit those specified by omit.
           Arguments omit or axes  may be as specified in axisMatchAxis
           order is an optional string determining the output order
        """
        alist = [d[0] for d in self.getDomain()]
        return axisMatchAxis (alist, axes, omit, order)

    def getAxisIds(self):
        "Get a list of axis identifiers"
        return [x[0].id for x in self.getDomain()]

    # Return the grid
    def getGrid(self):
        return self._grid_

    def getMissing(self, asarray=0):
        """Return the missing value as a scalar, or as
        a numpy array if asarray==1"""
        mv = self.missing_value
        if mv is None and hasattr(self,'_FillValue'):
            mv = self._FillValue
            
        if asarray==0 and isinstance(mv, numpy.ndarray):
            mv = mv[0]
        if type(mv) is types.StringType and self.dtype.char not in ['?','c','O','S']:
            mv = float(mv)
        return mv

    def _setmissing(self, name, value):
        self.setMissing(value)

    def setMissing(self, value):
        """Set the missing value, which may be a scalar,
        a single-valued numpy array, or None. The value is
        cast to the same type as the variable."""

        # Check for None first, so that constructors can
        # set missing_value before typecode() is initialized.
        if value is None:
            self._basic_set('missing_value', value)
            return
            
        selftype = self.typecode()
        valuetype = type(value)
        if valuetype is numpy.ndarray:
            value = value.astype(selftype).item()
        elif isinstance(value, numpy.floating) or isinstance(value, numpy.integer):
            value = numpy.array([value], selftype)
        elif valuetype in [types.FloatType, types.IntType, types.LongType, types.ComplexType]:
            try:
                value = numpy.array([value], selftype)
            except:                     # Set fill value when ar[i:j] returns a masked value
                value = numpy.array([numpy.ma.default_fill_value(self)], selftype)
        elif valuetype is types.StringType and selftype in ['?','c','O','S']: # '?' for Boolean and object
            pass
        else:
            raise CDMSError, 'Invalid missing value %s'%`value`

        self._missing = value


    def getTime(self):
        "Get the first time dimension, or None if not found"
        for k in range(self.rank()):
            axis = self.getAxis(k)
            if axis.isTime():
                return axis
                break
        else:
            return None

    def getForecastTime(self):
        "Get the first forecast time dimension, or None if not found"
        for k in range(self.rank()):
            axis = self.getAxis(k)
            if axis.isForecast():
                return axis
                break
        else:
            return None
    def getForecast(self):
        return self.getForecastTime()

    def getLevel(self):
        """Get the first vertical level dimension in the domain, 
           or None if not found.
        """
        for k in range(self.rank()):
            axis = self.getAxis(k)
            if axis.isLevel():
                return axis
                break
        else:
            return None

    def getLatitude(self):
        "Get the first latitude dimension, or None if not found."
        grid = self.getGrid()
        if grid is not None:
            result = grid.getLatitude()
        else:
            result = None
            
        if result is None:
            for k in range(self.rank()):
                result = self.getAxis(k)
                if result.isLatitude():
                    break
            else:
                result = None
                
        return result

    def getLongitude(self):
        "Get the first latitude dimension, or None if not found."
        grid = self.getGrid()
        if grid is not None:
            result = grid.getLongitude()
        else:
            result = None
            
        if result is None:
            for k in range(self.rank()):
                result = self.getAxis(k)
                if result.isLongitude():
                    break
            else:
                result = None
                
        return result


    # Get an order string, such as "tzyx"
    def getOrder(self, ids=0):
        """getOrder(ids=0) returns the order string, such as tzyx.

         if ids == 0 (the default) for an axis that is not t,z,x,y
         the order string will contain a '-' in that location.
         The result string will be of the same length as the number 
         of axes. This makes it easy to loop over the dimensions.

         if ids == 1 those axes will be represented in the order
         string as (id) where id is that axis' id. The result will
         be suitable for passing to order2index to get the 
         corresponding axes, and to orderparse for dividing up into
         components.
        """
        order = ""
        for k in range(self.rank()):
            axis = self.getAxis(k)
            if axis.isLatitude():
                order = order+"y"
            elif axis.isLongitude():
                order = order+"x"
            elif axis.isLevel():
                order = order+"z"
            elif axis.isTime():
                order = order+"t"
            elif ids:
                order = order + '(' + axis.id + ')'
            else:
                order = order + "-"
        return order

    def subSlice (self, *specs, **keys):
        speclist = self._process_specs (specs, keys)
        numericSqueeze = keys.get('numericSqueeze',0)

        # Get a list of single-index specs
        if numericSqueeze:
            singles = self._single_specs(specs)
        else:
            singles = None
        slicelist = self.specs2slices(speclist,force=1)
        d = self.expertSlice (slicelist)
        squeeze = keys.get ('squeeze', 0)
        raw = keys.get('raw',0)
        order = keys.get('order', None)
        grid = keys.get('grid', None)
        forceaxes = keys.get('forceaxes', None) # Force result to have these axes
        raweasy = raw==1 and order is None and grid is None
        if not raweasy:
            if forceaxes is None:
                axes = []
                allaxes = [None]*self.rank()
                for i in range(self.rank()):
                   slice = slicelist[i]
                   if squeeze and numpy.ma.size(d, i) == 1:
                       continue
                   elif numericSqueeze and i in singles:
                       continue
                   # Don't wrap square-bracket slices
                   axis = self.getAxis(i).subaxis(slice.start, slice.stop, slice.step, wrap=(numericSqueeze==0))
                   axes.append(axis)
                   allaxes[i] = axis
            else:
                axes = forceaxes

            # Slice the grid, if non-rectilinear. Don't carry rectilinear grids, since
            # they can be inferred from the domain.
            selfgrid = self.getGrid()
            if selfgrid is None or isinstance(selfgrid, AbstractRectGrid):
                resultgrid = None
            else:
                alist = [item[0] for item in self.getDomain()]
                gridslices, newaxes = selfgrid.getGridSlices(alist, allaxes, slicelist)

                # If one of the grid axes was squeezed, the result grid is None
                if None in newaxes:
                    resultgrid = None
                else:
                    resultgrid = apply(selfgrid.subSlice, gridslices, {'forceaxes': newaxes})

        resultArray = self._returnArray(d, squeeze, singles=singles)
        if self.isEncoded():
            resultArray  = self.decode(resultArray)
            newmissing = resultArray.fill_value
        else:
            newmissing = self.getMissing()

        if raweasy:
            return resultArray
        elif len(axes)>0:

            # If forcing use of input axes, make sure they are not copied.
            # Same if the grid is not rectilinear - this is when forceaxes is set.
            copyaxes = (forceaxes is None) and (resultgrid is None)
            result = TransientVariable(resultArray, 
                                     copy=0,
                                     fill_value = newmissing,
                                     axes=axes,
                                     copyaxes = copyaxes,
                                     grid = resultgrid,
                                     attributes=self.attributes,
                                     id = self.id)
            if grid is not None:
                order2 = grid.getOrder()
                if order is None:
                    order = order2
                elif order != order2:
                    raise CDMSError, 'grid, order options not compatible.'
            result = result.reorder(order).regrid(grid)
            if raw == 0:
                return result
            else:
                return result.getSlice(squeeze=0, raw=1)

        else:               # Return numpy.ma for zero rank, so that __cmp__ works.
            return resultArray

    def getSlice (self, *specs, **keys):
        """x.getSlice takes arguments of the following forms and produces
           a return array. The keyword argument squeeze determines whether
           or not the shape of the returned array contains dimensions whose
           length is 1; by default this argument is 1, and such dimensions
           are 'squeezed out'.
           There can be zero or more positional arguments, each of the form:
           (a) a single integer n, meaning slice(n, n+1)
           (b) an instance of the slice class
           (c) a tuple, which will be used as arguments to create a slice
           (d) None or ':', which means a slice covering that entire dimension
           (e) Ellipsis (...), which means to fill the slice list with ':'
               leaving only enough room at the end for the remaining
               positional arguments
           There can be keyword arguments of the form key = value, where
           key can be one of the names 'time', 'level', 'latitude', or
           'longitude'. The corresponding value can be any of (a)-(d) above.

           There must be no conflict between the positional arguments and
           the keywords.

           In (a)-(c) negative numbers are treated as offsets from the end
           of that dimension, as in normal Python indexing.
        """
        # Turn on squeeze and raw options by default.
        keys['numericSqueeze'] = keys.get('numericSqueeze',0)
        keys['squeeze'] = keys.get('squeeze',1-keys['numericSqueeze'])
        keys['raw'] = keys.get('raw',1)
        keys['order'] = keys.get('order', None)
        keys['grid'] = keys.get('grid', None)
        isitem = keys.get('isitem', 0)
        result = self.subSlice(*specs, **keys)

        # return a scalar for 0-D slices
        if isitem and result.size==1 and (not _numeric_compatibility) and not result.mask.item():
            result = result.item()
        return result

    def expertSlice(self, slicelist):
        raise CDMSError, NotImplemented + 'expertSlice'

    def getRegion(self, *specs, **keys):
        """getRegion
        Read a region of data. A region is an n-dimensional
        rectangular region specified in coordinate space.
        'slices' is an argument list, each item of which has one of the following forms:
        - x, where x is a scalar
          Map the scalar to the index of the closest coordinate value
        - (x,y)
          Map the half-open coordinate interval [x,y) to index interval
        - (x,y,'cc')
          Map the closed interval [x,y] to index interval. Other options are 'oo' (open),
          'oc' (open on the left), and 'co' (open on the right, the default).
        - (x,y,'co',cycle)
           Map the coordinate interval with wraparound. If no cycle is specified, wraparound
           will occur iff axis.isCircular() is true.
           NOTE: Only one dimension may be wrapped.
        - Ellipsis
           Represents the full range of all dimensions bracketed by non-Ellipsis items.
        - ':' or None
           Represents the full range of one dimension.

        For example, suppose the variable domain is (time,level,lat,lon). Then

                 getRegion((10,20),850,Ellipsis,(-180,180))

        retrieves:
            - all times t such that 10.<=t<20.
            - level 850
            - all values of all dimensions between level and lon (namely, lat)
            - longitudes x such that -180<=x<180. This will be wrapped unless
              lon.topology=='linear'
        """

        # By default, squeeze and raw options are on
        keys['squeeze'] = keys.get ('squeeze', 1)
        keys['raw'] = keys.get('raw',1)
        keys['order'] = keys.get('order', None)
        keys['grid'] = keys.get('grid', None)
        return self.subRegion(*specs, **keys)

    def subRegion (self, *specs, **keys):

        speclist = self._process_specs (specs, keys)
        slicelist = self.reg_specs2slices (speclist)

        squeeze = keys.get ('squeeze', 0)
        raw = keys.get('raw',0)
        order = keys.get('order', None)
        grid = keys.get('grid', None)
        raweasy = raw==1 and order is None and grid is None
        if grid is not None and order is None:
            order = grid.getOrder()


        # Check if any slice wraps around.

        wrapdim = -1
        
        axes = []

        circulardim = None
        
        for idim in range(len(slicelist)):
            item = slicelist[idim]
            axis = self.getAxis(idim)
            axislen = len(axis)

            if(axis.isCircular()): circulardim=idim

            wraptest1 = ( axis.isCircular() and speclist[idim] != unspecified)
            start, stop = item.start, item.stop
            wraptest2 = not ((start is None or (0<=start<axislen)) and (stop is None or (0<=stop<=axislen)))

            if ( wraptest1 and wraptest2):
                if wrapdim >= 0:
                    raise CDMSError, "Too many dimensions wrap around."
                wrapdim = idim
                break
                    
        else:

            # No wraparound, just read the data

            # redo the speclist -> slice if passed circular test but not wrapped test

            if(circulardim is not None):
                slicelist = self.reg_specs2slices (speclist,force=circulardim)
                
            d = {'raw':raw, 
                 'squeeze':squeeze,
                 'order':order,
                 'grid':grid,
                }
            return self.subSlice(*slicelist, **d)

        #
        #  get the general wrap slice (indices that are neg -> pos and vica versa)
        #

        wrapslice = slicelist[wrapdim]
        wrapaxis = self.getAxis(wrapdim)
        length = len(wrapaxis)

        #
        # shift the wrap slice to the positive side and calc number of cycles shifted
        #

        wb=wrapslice.start
        we=wrapslice.stop
        ws=wrapslice.step
        size=length
        cycle=self.getAxis(wrapdim).getModulo()

        #
        # ncycle:
        #   number of cycles for slicing purposes and
        #   resetting world coordinate from the positive only direction
        #
        # ncyclesrev:
        #    resetting the world coordinate for reversed direction
        #
        
        ncycles=0
        ncyclesrev=0

        if(ws>0):

            if(wb>0):
                ncycles=1
                while(wb>=0):
                    wb=wb-size
                    we=we-size
                    ncycles=ncycles-1
            else:
                ncycles=0
                while(wb<0):
                    wb=wb+size
                    we=we+size
                    ncycles=ncycles+1
                    
            if(wb < 0):
                wb=wb+size
                we=we+size
                
        #  reversed direction
        
        else:

            # do the ncycles for resetting world coordinate
            wbrev=wb
            werev=we
            werevNoneTest=0
            if(werev is None):
                werev=0
                werevNoneTest=1

            ncycleRevStart=1
            if(wbrev > 0):
                ncyclesrev=ncycleRevStart
                while(wbrev>=0):
                    wbrev=wbrev-size
                    werev=werev-size
                    ncyclesrev=ncyclesrev-1
            else:
                ncyclesrev=0
                while(wbrev<0):
                    wbrev=wbrev+size
                    werev=werev+size
                    ncyclesrev=ncyclesrev+1

            while(werev < 0):
                wbrev=wbrev+size
                werev=werev+size

            # number of cycles to make the slice positive
            while( we<0 and we != None ):
                wb=wb+size
                we=we+size
                ncycles=ncycles+1

            wb=wbrev
            we=werev
            if(werevNoneTest): we=None
            
        wrapslice=slice(wb,we,ws)
        
        #
        #  calc the actual positive slices and create data array
        #

        donew=1

        if(donew):

            wraps = splitSliceExt(wrapslice, length)

            for kk in range(0,len(wraps)):
                sl=wraps[kk]
                
                slicelist[wrapdim] = sl

                if(kk == 0):
                    ar1 = self.getSlice(squeeze=0, *slicelist)
                    result=ar1
                else:
                    ar2 = self.getSlice(squeeze=0, *slicelist)
                    result = numpy.ma.concatenate((result,ar2),axis=wrapdim)

        else:

            wrap1, wrap2 = splitSlice(wrapslice, length)
            slicelist[wrapdim] = wrap1
            ar1 = self.getSlice(squeeze=0, *slicelist)
            slicelist[wrapdim] = wrap2
            ar2 = self.getSlice(squeeze=0, *slicelist)
            result = numpy.ma.concatenate((ar1,ar2),axis=wrapdim)


        if raweasy:
            return self._returnArray(result, squeeze)

        #----------------------------------------------------------------------
        #
        #  set ALL the axes (transient variable)
        #
        #----------------------------------------------------------------------

        wrapspec=speclist[wrapdim]
        
        axes = []
        for i in range(self.rank()):
            if squeeze and numpy.ma.size(result, i) == 1: continue

            sl = slicelist[i]

            if i == wrapdim:

                axis = self.getAxis(i).subAxis(wb, we, ws)
                
                if(ws > 0):
                    delta_beg_wrap_dimvalue = ncycles*cycle
                else:
                    delta_beg_wrap_dimvalue = ncyclesrev*cycle

                axis.setBounds(axis.getBounds() - delta_beg_wrap_dimvalue)
                
                axis[:]= (axis[:] - delta_beg_wrap_dimvalue).astype(axis.typecode())

            else:
                axis = self.getAxis(i).subaxis(sl.start, sl.stop, sl.step)
            axes.append(axis)

        result = self._returnArray(result, squeeze)
        result = TransientVariable(result,
                                 copy=0, 
                                 fill_value = self.missing_value,
                                 axes=axes,
                                 attributes=self.attributes,
                                 id = self.id)
        if grid is not None:
            order2 = grid.getOrder()
            if order is None:
                order = order2
            elif order != order2:
                raise CDMSError, 'grid, order options not compatible.'
            
        result = result.reorder(order).regrid(grid)
        if raw == 0:
            return result
        else:
            return result.getSlice(squeeze=0, raw=1)

    def getValue(self, squeeze=1):
        """Return the entire set of values."""
        return self.getSlice(Ellipsis, squeeze=squeeze)
    
    def assignValue(self,data):
        raise CDMSError, NotImplemented + 'assignValue'

    def reorder (self, order):
        """return self reordered per the specification order"""
        if order is None: 
            return self
        axes = self.getAxisList()
        permutation = order2index(axes, order)
        if permutation == range(len(axes)):
            return self
        return MV.transpose (self, permutation)

    def regrid (self, togrid, missing=None, order=None, mask=None):
        """return self regridded to the new grid. Keyword arguments
        are as for regrid.Regridder."""
        from regrid2 import Regridder

        if togrid is None: 
            return self
        else:
            fromgrid = self.getGrid()
            regridf = Regridder(fromgrid, togrid)
            result = regridf(self, missing=missing, order=order, mask=mask)
            return result

    def pressureRegrid (self, newLevel, missing=None, order=None, method="log"):
        """Return the variable regridded to new pressure levels.
        The variable should be a function of lat, lon, pressure, and (optionally) time.
        <newLevel> is an axis of the result pressure levels.
        <method> is optional, either "log" to interpolate in the log of pressure (default),
          or "linear" for linear interpolation.
        <missing> and <order> are as for regrid.PressureRegridder.
        """
        from regrid2 import PressureRegridder

        fromlevel = self.getLevel()
        if fromlevel is None:
            raise CDMSError, 'No pressure level'
        pregridf = PressureRegridder(fromlevel, newLevel)
        result = pregridf(self, missing=missing, order=order, method=method)
        return result

    def crossSectionRegrid(self, newLevel, newLatitude, missing=None, order=None, method="log"):
        """Return the variable regridded to new pressure levels and latitudes.
        The variable should be a function of lat, level, and (optionally) time.
        <newLevel> is an axis of the result pressure levels.
        <newLatitude> is an axis of latitude values.
        <method> is optional, either "log" to interpolate in the log of pressure (default),
          or "linear" for linear interpolation.
        <missing> and <order> are as for regrid.CrossSectionRegridder.
        """
        from regrid2 import CrossSectionRegridder

        fromlevel = self.getLevel()
        fromlat = self.getLatitude()
        if fromlevel is None:
            raise CDMSError, 'No pressure level'
        if fromlat is None:
            raise CDMSError, 'No latitude level'
        xregridf = CrossSectionRegridder(fromlat, newLatitude, fromlevel, newLevel)
        result = xregridf(self, missing=missing, order=order, method=method)
        return result

    def _process_specs (self, specs, keys):
        """Process the arguments for a getSlice, getRegion, etc.
           Returns an array of specifications for all dimensions.
           Any Ellipsis has been eliminated.
           time, level, latitude, longitude keywords handled here
        """
        myrank = self.rank()
        nsupplied = len(specs)
        if Ellipsis in specs:
            nellipses = 1
        elif numpy.newaxis in specs:
            raise CDMSError, 'Sorry, you cannot use NewAxis in this context ' + str(specs)
        else:
            nellipses = 0
        if nsupplied-nellipses > myrank:
            raise CDMSError, InvalidRegion + \
              "too many dimensions: %d, for variable %s"%(len(specs),self.id)

        speclist = [unspecified]*myrank
        i = 0
        j = 0
        while i < nsupplied:
            if specs[i] is Ellipsis:
               j = myrank  - (nsupplied - (i+1)) 
            else:
               speclist[j] = specs[i]
               j = j + 1
            i = i + 1

        for k, v in keys.items():
            if k in ['squeeze','raw','grid','order']: 
                continue
            i = self.getAxisIndex(k)
            if i >= 0:
                if speclist[i] is not unspecified:
                    raise CDMSError, 'Conflict between specifier %s and %s'%(`speclist[i]`,`keys`)
                speclist[i] = v

        return speclist

    def _single_specs (self, specs):
        """Return a list of dimension indices where the spec is an index."""
        myrank = self.rank()
        nsupplied = len(specs)
        i = 0
        j = 0
        singles = []
        while i < nsupplied:
            if specs[i] is Ellipsis:
                j = myrank  - (nsupplied - (i+1)) 
            else:
                if isinstance(specs[i], types.IntType):
                    singles.append(j)
                j = j + 1
            i = i + 1
        return singles

    def specs2slices (self, speclist, force=None):
        """Create an equivalent list of slices from an index specification
           An index specification is a list of acceptable items, which are
           -- an integer
           -- a slice instance (slice(start, stop, stride))
           -- the object "unspecified"
           -- the object None
           -- a colon
           The size of the speclist must be self.rank()
        """
        if len(speclist) != self.rank():
            raise CDMSError, "Incorrect length of speclist in specs2slices."
        slicelist = []
        for i in range(self.rank()):
            key = speclist[i]
            if isinstance(key, types.IntType):  # x[i]
                slicelist.append (slice(key,key+1))
            elif type(key) is types.SliceType: # x[i:j:k]
                slicelist.append(key)
            elif key is unspecified or key is None or key == ':':
                slicelist.append (slice(0, len(self.getAxis(i))))
            elif key is Ellipsis:
                raise CDMSError, "Misuse of ellipsis in specification."
            elif type(key) is types.TupleType:
                slicelist.append(slice(*key))
            else:
                raise CDMSError, 'invalid index: %s'% str(key)
        # Change default or negative start, stop to positive
        for i in range(self.rank()):
            axis = self.getAxis(i)
            length = len(axis)
            start = slicelist[i].start
            stop = slicelist[i].stop
            step = slicelist[i].step
            #
            # allow negative indices in a wrapped (isCircular() = 1) axis
            #
            circular=(axis.isCircular() and force is None)

            altered = 0
            if step is None: 
                altered = 1
                step=1

            if ( ( start is None or stop is None or start<0 or stop<0 ) and ( circular == 0 ) ):
                altered = 1
                adjustit = 1
                if step>0:
                    if start is None: 
                        start=0
                    if stop is None: 
                        stop=length
                    if start==-1 and stop==0:
                        stop=length
                else:
                    if start is None: 
                        start=length-1
                    if stop is None:
                        # stop=-1
                        adjustit = 0
                if start<0: 
                    start=start%length
                if stop<0 and adjustit: 
                    stop=stop%length
            if altered: 
                slicelist[i] = slice(start, stop, step)
        return slicelist

    def reg_specs2slices(self, initspeclist,force=None):

        # Don't use input to store return value
        speclist=copy.copy(initspeclist)

        for i in range(self.rank()):
            item = speclist[i]
            if isinstance(item, types.SliceType):
                newitem = item
            elif item==':' or item is None or item is unspecified:
                axis = self.getAxis(i)
                newitem = slice(0,len(axis))
            elif isinstance(item, types.ListType) or \
                 isinstance(item, types.TupleType):
                axis = self.getAxis(i)
                if len(item)==2:        # (start,end)
                    indexInterval = axis.mapIntervalExt(item)
                elif len(item)==3:      # (start,end,'xxx')
                    coordInterval = (item[0],item[1])
                    indexInterval = axis.mapIntervalExt(coordInterval,item[2])
                elif len(item)==4:
                    coordInterval = (item[0],item[1])
                    indexInterval = axis.mapIntervalExt(coordInterval,item[2],item[3])
                elif len(item)==5:
                    coordInterval = (item[0],item[1])
                    indexInterval = axis.mapIntervalExt(coordInterval,item[2],item[3],item[4])
                elif len(item)==6:
                    coordInterval = (item[0],item[1])
                    indexInterval = axis.mapIntervalExt(coordInterval,item[2],item[3],item[4],item[5])
                else:
                    raise CDMSError, InvalidRegion + "invalid format for coordinate interval: %s"%str(item)
                if indexInterval is None:
                    raise CDMSError, OutOfRange + str(item)
                newitem = slice(indexInterval[0],indexInterval[1],indexInterval[2])
            elif isinstance(item, numpy.floating) or \
                 isinstance(item, types.FloatType) or \
                 isinstance(item, numpy.integer) or \
                 isinstance(item, types.IntType) or \
                 isinstance(item, types.LongType) or \
                 isinstance(item, types.StringType) or \
                 type(item) in CdtimeTypes:
                axis = self.getAxis(i)
                #
                # default is 'ccn' in axis.mapIntervalExt
                #
                indexInterval = axis.mapIntervalExt((item,item))
                if indexInterval is None:
                    raise CDMSError, OutOfRange + str(item)
                newitem = slice(indexInterval[0],indexInterval[1],indexInterval[2])
            else:
                raise CDMSError, InvalidRegion + "invalid format for coordinate interval: %s"%str(item)

            speclist[i] = newitem

        slicelist = self.specs2slices(speclist,force)
        return slicelist

    def _decodedType(self):
        "The datatype after decoding."
        if hasattr(self, 'scale_factor') and isinstance(self.scale_factor, numpy.ndarray):
            result = self.scale_factor.dtype.char
        elif hasattr(self, 'add_offset') and isinstance(self.add_offset, numpy.ndarray):
            result = self.add_offset.dtype.char
        else:
            result = numpy.float32
        return result

    def isEncoded(self):
        "True iff self is represented as packed data."
        return (hasattr(self,"scale_factor") or hasattr(self,"add_offset"))

    def decode(self, ar):
        "Decode compressed data. ar is a masked array, scalar, or numpy.ma.masked"
        resulttype = self._decodedType()
        if hasattr(self, 'scale_factor'):
            scale_factor = self.scale_factor
        else:
            scale_factor = numpy.array([1.0],resulttype)
            
        if hasattr(self, 'add_offset'):
            add_offset = self.add_offset
        else:
            add_offset = numpy.array([0.0],resulttype)
            
        if ar is not numpy.ma.masked:
            result = scale_factor*ar + add_offset
            if isinstance(result,numpy.ma.MaskedArray):
                result = result.astype(resulttype)
                numpy.ma.set_fill_value(result, numpy.ma.default_fill_value(0.))
            else:
                tmp = numpy.array(result)
                result = tmp.astype(resulttype)[0]
            return result
        else:
            return ar

    def getGridIndices(self):
        """Return a tuple of indices corresponding to the variable grid."""
        grid = self.getGrid()
        result = []
        if grid is not None:
            gridaxes = grid.getAxisList()
            varaxes = [d[0] for d in self.getDomain()]

            for i in range(len(gridaxes)):
                for j in range(len(varaxes)):
                    if gridaxes[i] is varaxes[j]:
                        result.append(j)
                        break
                else:
                    raise CDMSError, 'Variable and grid do not share common dimensions: %s'%self.id

        return tuple(result)

    # numpy.ma overrides

    def __getitem__(self, key):
        if type(key) is types.TupleType:
            speclist = self._process_specs(key, {})
        else:
            if isinstance(key, types.IntType) and key>=len(self):
                raise IndexError, "Index too large: %d"%key
            speclist = self._process_specs([key], {})

        # Note: raw=0 ensures that a TransientVariable is returned
        return self.getSlice(numericSqueeze=1, raw=0, isitem=1, *speclist)
        
    def __getslice__(self, low, high):

        # Note: raw=0 ensures that a TransientVariable is returned
        return self.getSlice (slice(low, high), numericSqueeze = 1, raw=0)

    def typecode(self):
        raise CDMSError, NotImplemented + 'typecode'

    def __abs__(self): 
        return MV.absolute(self)

    def __neg__(self): 
        return MV.negative(self)

    def __add__(self, other):
        return MV.add(self, other)
                        
    __radd__ = __add__

    def __lshift__ (self, n):
        return MV.left_shift(self, n)

    def __rshift__ (self, n):
        return MV.right_shift(self, n)
                        
    def __sub__(self, other): 
        return MV.subtract(self, other)

    def __rsub__(self, other): 
        return MV.subtract(other, self)

    def __mul__(self, other):
        return MV.multiply(self, other)
    
    __rmul__ = __mul__

    def __div__(self, other): 
        return MV.divide(self, other)

    def __rdiv__(self, other): 
        return MV.divide(other, self)

    def __pow__(self,other, third=None): 
        return MV.power(self, other, third)

    def __iadd__(self, other): 
        "Add other to self in place."
        return MV.add(self, other)   # See if we can improve these later.

    def __isub__(self, other): 
        "Subtract other from self in place."
        return MV.subtract(self, other)   # See if we can improve these later.

    def __imul__(self, other): 
        "Multiply self by other in place."
        return MV.multiply(self, other)   # See if we can improve these later.

    def __idiv__(self, other): 
        "Divide self by other in place."
        return MV.divide(self, other)   # See if we can improve these later.

    def __eq__(self,other): 
        return MV.equal(self,other)

    def __ne__(self,other): 
        return MV.not_equal(self,other)

    def __lt__(self,other): 
        return MV.less(self,other)

    def __le__(self,other): 
        return MV.less_equal(self,other)

    def __gt__(self,other): 
        return MV.greater(self,other)

    def __ge__(self,other): 
        return MV.greater_equal(self,other)

    def __sqrt__(self): 
        return MV.sqrt(self)

    def astype (self, tc):
        "return self as array of given type."
        return self.subSlice().astype(tc)
    

## internattr.add_internal_attribute(AbstractVariable, 'id', 'parent')   
#PropertiedClasses.set_property(AbstractVariable, 'missing_value', acts=AbstractVariable._setmissing, nodelete=1)

__rp = r'\s*([-txyz0-9]{1,1}|\(\s*\w+\s*\)|[.]{3,3})\s*'
__crp = re.compile(__rp)
def orderparse (order):
    """Parse an order string. Returns a list of axes specifiers.
       Order elements can be:
          Letters t, x, y, z meaning time, longitude, latitude, level
          Numbers 0-9 representing position in axes
          The letter - meaning insert the next available axis here.
          The ellipsis ... meaning fill these positions with any
            remaining axes.
          (name) meaning an axis whose id is name
    """
    if not isinstance(order, types.StringType):
        raise CDMSError, 'order arguments must be strings.'
    pos = 0
    result=[]
    order = order.strip()
    while pos < len(order):
        m = __crp.match(order, pos)
        if m is None: break
        r = m.group(1)
        if r[0] == '(':
            pass
        elif r == '...':
            r = Ellipsis
        elif len(r) == 1:
            if r in string.digits:
                r = string.atoi(r)
        result.append(r)
        pos = m.end(0)

    if pos != len(order):
        raise CDMSError, 'Order string "' + order + \
                          '" malformed, index '+str(pos)
    return result

def order2index (axes, order):
    """Find the index permutation of axes to match order.
       The argument order is a string.
       Order elements can be:
          Letters t, x, y, z meaning time, longitude, latitude, level
          Numbers 0-9 representing position in axes
          The letter - meaning insert the next available axis here.
          The ellipsis ... meaning fill these positions with any
            remaining axes.
          (name) meaning an axis whose id is name
    """
    if isinstance(order, types.StringType):
        result = orderparse(order)
    elif isinstance(order, types.ListType):
        result = order
    else:
        raise CDMSError, 'order2index, order specified of bad type:' + str(type(order))
    n = len(axes)
    ie = n
    permutation = [None]*n
    j = 0
    pos = 0
    while j < len(result):
        item = result[j]
        if isinstance(item, types.StringType):
            if item == 't': 
                spec = 'time'
            elif item == 'x': 
                spec = 'longitude'
            elif item == 'y': 
                spec = 'latitude'
            elif item == 'z': 
                spec = 'level'
            elif item == '-':
                pos += 1
                j += 1
                continue
            else:
                spec = item[1:-1]
            for k in range(n):
                if axisMatches(axes[k], spec):
                    if k in permutation:
                        raise CDMSError, 'Duplicate item in order %s' % order
                    permutation[pos] = k
                    pos += 1
                    break
            else:
                raise CDMSError, 'No axis matching order spec %s' %str(item)
        elif isinstance(item, types.IntType):
            if item in permutation:
                raise CDMSError, 'Duplicate item in order %s' % order
            if item >= n:
                raise CDMSError, 'Index %d out of range in order %s' %\
                                 (item,order)
            permutation[pos] = item
            pos += 1
        elif item is Ellipsis:
            nleft = len(result) - j - 1
            pos = n - nleft
        else:
            raise CDMSError, 'List specified for order contains bad item: ' + repr(item)
        j += 1

    for i in range(n):
        if i not in permutation:
            for j in range(n):
                if permutation[j] is None:
                    permutation[j] = i
                    break
    return permutation    

from tvariable import TransientVariable
import MV2 as MV
