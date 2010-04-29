"""
TransientVariable (created by createVariable)
is a child of both AbstractVariable and the masked array class.
Contains also the write part of the old cu interface.
"""
import types
import MA, Numeric, PropertiedClasses
from error import CDMSError
from avariable import AbstractVariable

from axis import createAxis, AbstractAxis
from grid import createRectGrid, AbstractRectGrid
from hgrid import AbstractCurveGrid
from gengrid import AbstractGenericGrid

id_builtin = id                         # built_in gets clobbered by keyword

class TransientVariable(AbstractVariable, MA.array):
    "An in-memory variable."
    count = 0

    def __init__(self, data, typecode=None, copy=0, savespace=0, 
                 mask=None, fill_value=None, grid=None,
                 axes=None, attributes=None, id=None, copyaxes=1):
        """createVariable (self, data, typecode=None, copy=0, savespace=0, 
                 mask=None, fill_value=None, grid=None,
                 axes=None, attributes=None, id=None)
           data is the data or a *tuple* giving the shape (not a list!)
        """
        if type(data) is types.TupleType:
            data = list(data)
        if isinstance(data, AbstractVariable):
            if not isinstance(data, TransientVariable):
                data = data.subSlice()
            if attributes is None: attributes = data.attributes
            if axes is None: axes = map(lambda x: x[0], data.getDomain())
            if typecode is None: typecode=data.typecode()
            if grid is None:
                grid = data.getGrid()
                if (grid is not None) and (not isinstance(grid, AbstractRectGrid)) and (not grid.checkAxes(axes)):
                    grid = grid.reconcile(axes) # Make sure grid and axes are consistent
        if isinstance(data, MA.array):
            if fill_value is None: fill_value = data.fill_value()
        MA.array.__init__(self, data, typecode = typecode, 
                                copy = copy,
                                savespace = savespace, 
                                mask = mask,
                                fill_value = fill_value)
        AbstractVariable.__init__ (self)

        # Initialize the geometry
        if grid is not None:
            copyaxes=0                  # Otherwise grid axes won't match domain.
        self.initDomain(axes, copyaxes=copyaxes)           # Note: clobbers the grid, so set the grid after.
        if grid is not None:
            self.setGrid(grid)
 
        # Initialize attributes
        fv = self.fill_value()
        if attributes is not None:
            for key, value in attributes.items():
                if key in ['shape','flat','imaginary','real'] or key[0]=='_':
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
            if type(id) is not types.StringType:
                raise CDMSError, 'id must be a string'
            self.id = id
        elif hasattr(data,'id'):
            self.id = data.id

        if self.id is None:
            TransientVariable.count = TransientVariable.count + 1
            self.id = 'variable_' + str(TransientVariable.count)
        self.name = getattr(self, 'name', self.id)

    typecode = MA.array.typecode

    def assignValue(self,data):
        self[...] = data

    def getValue(self, squeeze=1):
        return self.filled()

    def expertSlice (self, slicelist):
        return MA.array.__getitem__(self, slicelist)

    def initDomain (self, axes, copyaxes=1):
        # lazy evaluation via getAxis to avoid creating axes that aren't ever used.
        newgrid = None
        self.__domain = [None]*self.rank()
        if axes is not None:
            flataxes = []
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
                    raise CDMSError, "Invalid item in axis list: "+`item`
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
            length = self.size(n)
            axis = createAxis(MA.arange(self.size(n), typecode=MA.Float))
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
        maresult = MA.MaskedArray.astype(self,tc)
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
            bigmask = Numeric.resize(mask, newshape)

            # Generate the tranpose vector
            t = tuple(tprep + list(gridindices))
            tinv = [0]*len(t)
            for i in range(len(t)):
                tinv[t[i]] = i

            # And reshape to fit the variable
            if tinv!=range(len(tinv)):
                bigmask = Numeric.transpose(bigmask, tuple(tinv))

        else:
            bigmask = mask

        # Apply the mask to self
        currentmask = self.mask()
        if currentmask is not None:
            bigmask = Numeric.logical_or(currentmask, bigmask)

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
            a = createAxis(MA.filled(value[:]))
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
               b = MA.filled(value)
               if MA.rank(b) == 2:
                   d.setBounds(b)
               elif MA.rank(b) == 1:
                   b1 = Numeric.zeros((len(b)-1,2), b.typecode())
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

##     def __str__ (self):
##         # return "<TransientVariable %s%s, type = %s, has %d elements>"%(self.id, `self.shape`, self.typecode(), self.size())
##         return MA.array.__str__(self)

    def __repr__ (self):
        return self.id + '\n' + MA.array.__repr__(self) + '\n'

    def set_fill_value(self, value):
        "Set missing value attribute and fill value"
        AbstractVariable.setMissing(self, value)
        self.__dict__['_fill_value'] = self.missing_value

    def setMissing (self, value):
        "Set missing value attribute and fill value"
        self.set_fill_value(value)

    # For aggregation server interface. Use clone to make a true copy.
    def copy(self):
        return self.filled()

PropertiedClasses.set_property(TransientVariable, 'shape', 
                               nowrite=1, nodelete=1)

createVariable = TransientVariable

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
    
    if result.typecode() == MA.PyObject:
        result = None
    if result is None:
        raise CDMSError, "asVariable could not make a Variable from the input."
    return result

if __name__ == '__main__':
    import Numeric
    for s in [(20,), (4,5)]:
        x = Numeric.arange(20)
        x.shape = s
        t = createVariable(x)
        assert t.shape == s
        assert t.missing_value == t._fill_value
        assert MA.allclose(x, t)
        assert t.typecode() == MA.Int
        assert MA.size(t) == MA.size(x)
        assert MA.size(t,0) == len(t)
        assert MA.allclose(t.getAxis(0)[:], MA.arange(MA.size(t,0)))
        t.missing_value = -99
        assert t.missing_value == -99
        assert t.fill_value() == -99
    t = createVariable(MA.arange(5), mask=[0,0,0,1,0])
    t.set_fill_value (1000)
    assert t.fill_value() == 1000
    assert t.missing_value == 1000
    t.missing_value = -99
    assert t[2] == 2
    t[3] = MA.masked
    assert t[3] is MA.masked
    f = createVariable(MA.arange(5, typecode=MA.Float32), mask=[0,0,0,1,0])
    f2 = createVariable(MA.arange(5, typecode=MA.Float32), mask=[0,0,0,1,0])
    f[3] = MA.masked
    assert f[3] is MA.masked
    assert MA.allclose(2.0, f[2])
    t.setdimattribute(0, 'units', 'cm')
    assert t.getdimattribute(0, 'units') == 'cm'
    t.setdimattribute(0, 'name', 'fudge')
    assert t.getdimattribute(0, 'name') == 'fudge'
    f2b = f2.getdimattribute(0, 'bounds')
    t.setdimattribute(0, 'bounds', f2b)
    assert MA.allclose(f.getdimattribute(0,'bounds'), f2.getdimattribute(0,'bounds'))
    print "Transient Variable test passed ok."

