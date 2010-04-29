"CDMS Variable objects, MaskedArray interface"
import MA, Numeric
from Precision import *
from MA import allclose, allequal, common_fill_value, compress, create_mask, dot, e, fill_value, filled, \
     get_print_limit, getmask, getmaskarray, identity, indices, innerproduct, masked, pi, put, putmask, rank, ravel, \
     set_fill_value, set_print_limit, shape, size, isMA, isMaskedArray, is_mask, isarray, \
     make_mask, make_mask_none, mask_or
from avariable import AbstractVariable
from axis import allclose as axisAllclose, TransientAxis, concatenate as axisConcatenate, take as axisTake
from tvariable import TransientVariable, asVariable
from grid import AbstractRectGrid
from error import CDMSError

def _makeMaskedArg(x):
    """If x is a variable, turn it into a TransientVariable."""
    if isinstance(x, AbstractVariable) and not isinstance(x, TransientVariable):
        return x.subSlice()
    else:
        return x

def _extractMetadata(a, axes=None, attributes=None, id=None, omit=None):
    """Extract axes, attributes, id from 'a', if arg is None."""
    resultgrid = None
    if isinstance(a, AbstractVariable):
        if axes is None:
            axes = a.getAxisList(omit=omit)
        if attributes is None:
            attributes = a.attributes
        if id is None:
            id = a.id

        # If the grid is rectilinear, don't return an explicit grid: it's implicitly defined
        # by the axes.
        resultgrid = a.getGrid()
        if (resultgrid is None) or (isinstance(resultgrid, AbstractRectGrid)) or (axes is None):
            resultgrid = None

        # If the omitted axis was associated with the grid, the result will not be gridded.
        elif (omit is not None) and (resultgrid is not None) and (a.getAxis(omit) in resultgrid.getAxisList()):
            resultgrid = None

    return axes, attributes, id, resultgrid

class var_unary_operation:
    def __init__(self, mafunc):
        """ var_unary_operation(mafunc)
        mafunc is an MA masked_unary_function.
        """
        self.mafunc = mafunc
        self.__doc__ = mafunc.__doc__

    def __call__ (self, a):
        axes, attributes, id, grid = _extractMetadata(a)
        maresult = self.mafunc(_makeMaskedArg(a))
        return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def commonDomain(a,b,omit=None):
    """commonDomain(a,b) tests that the domains of variables/arrays a and b are equal,
    and returns the common domain if equal, or None if not equal. The domains may
    differ in that one domain may have leading axes not common
    to the other; the result domain will contain those axes.
    If <omit> is specified, as an integer i, skip comparison of the ith dimension
    and return None for the ith (common) dimension.
    """
    
    if isinstance(b, AbstractVariable):
        bdom = b.getAxisList()
    else:
        bdom = None
    return commonAxes(a,bdom,omit=omit)

def commonAxes(a,bdom,omit=None):
    """Helper function for commonDomain. 'a' is a variable or array,
    'b' is an axislist or None.
    """
    if isinstance(a, AbstractVariable) and bdom is not None:
        adom = a.getAxisList()
        arank = len(adom)
        brank = len(bdom)
        maxrank = max(arank,brank)
        minrank = min(arank,brank)
        diffrank = maxrank-minrank
        if maxrank==arank:
            maxdom = adom
        else:
            maxdom = bdom
        common = [None]*maxrank
        if omit is None:
            iomit = None
        else:
            iomit = omit-minrank

        # Check shared dimensions, last to first
        for i in range(minrank):
            j = -i-1
            if j==iomit:
                continue
            aj = adom[j]
            bj = bdom[j]
            if len(aj)!=len(bj):
                return None
            elif axisAllclose(aj,bj):
                common[j] = aj
            else:
                common[j] = TransientAxis(Numeric.arange(len(aj)))

        # Copy leading (non-shared) axes
        for i in range(diffrank):
            common[i]=maxdom[i]

        return common
    elif isinstance(a, AbstractVariable):
        common = a.getAxisList()
        if omit is not None:
            common[omit] = None
        return common
    else:
        if omit is not None:
            bdom[omit] = None
        return bdom

def commonGrid(a, b, axes):
    """commonGrid(a,b,axes) tests if the grids associated with variables a, b are equal,
    and consistent with the list of axes. If so, the common grid is returned, else None
    is returned. a and b can be Numeric arrays, in which case the result is None.

    The common grid is 'consistent' with axes if the grid axes (e.g., the axes of latitude and
    longitude coordinate variables) are members of the list 'axes'.

    If the grid(s) of a, b are rectilinear, the result is None, as the grids are implicitly
    defined by the axes.
    """
    if isinstance(b, AbstractVariable):
        gb = b.getGrid()
        if isinstance(gb, AbstractRectGrid):
            gb = None
    else:
        gb = None

    return commonGrid1(a, gb, axes)

def commonGrid1(a, gb, axes):
    """Helper function for commonGrid."""
    if isinstance(a, AbstractVariable):
        ga = a.getGrid()
        if isinstance(ga, AbstractRectGrid):
            ga = None
    else:
        ga = None

    if ga is gb:
        result = ga
    elif ga is None:                    # e.g., var*scalar
        result = gb
    elif gb is None:                    # e.g., scalar*var
        result = ga
    elif ga.isClose(gb):
        result = ga
    else:
        result = None

    if result is not None:
        for item in result.getAxisList():
            if item not in axes:
                result = None
                break

    return result

class var_binary_operation:
    def __init__(self, mafunc):
        """ var_binary_operation(mafunc)
        mafunc is an MA masked_binary_function.
        """
        self.mafunc = mafunc
        self.__doc__ = mafunc.__doc__

    def __call__ (self, a, b):
        axes = commonDomain(a,b)
        grid = commonGrid(a,b,axes)
        ta = _makeMaskedArg(a)
        tb = _makeMaskedArg(b)
        maresult = self.mafunc(ta,tb)
        return TransientVariable(maresult, axes=axes, grid=grid)

    def reduce (self, target, axis=0):
        ttarget = _makeMaskedArg(target)
        maresult = self.mafunc.reduce(ttarget, axis=axis)
        axes, attributes, id, grid = _extractMetadata(target, omit=axis)
        return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

    def accumulate (self, target, axis=0):
        ttarget = _makeMaskedArg(target)
        maresult = self.mafunc.accumulate(ttarget, axis=axis)
        axes, attributes, id, grid = _extractMetadata(target, omit=axis)
        return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)
        
    def outer (self, a, b):
        """Return the function applied to the outer product of a and b"""
        a1 = _makeMaskedArg(a)
        b1 = _makeMaskedArg(b)
        maresult = self.mafunc.outer(a1, b1)
        return TransientVariable(maresult)
        
sqrt = var_unary_operation(MA.sqrt)
log = var_unary_operation(MA.log)
log10 = var_unary_operation(MA.log10)
exp = var_unary_operation(MA.exp)
conjugate = var_unary_operation(MA.conjugate)
sin = var_unary_operation(MA.sin)
cos = var_unary_operation(MA.cos)
tan = var_unary_operation(MA.tan)
arcsin = var_unary_operation(MA.arcsin)
arccos = var_unary_operation(MA.arccos)
arctan = var_unary_operation(MA.arctan)
sinh = var_unary_operation(MA.sinh)
cosh = var_unary_operation(MA.cosh)
tanh = var_unary_operation(MA.tanh)
absolute = var_unary_operation(MA.absolute)
fabs = var_unary_operation(MA.fabs)
negative = var_unary_operation(MA.negative)
nonzero = var_unary_operation(MA.nonzero)
around = var_unary_operation(MA.around)
floor = var_unary_operation(MA.floor)
ceil = var_unary_operation(MA.ceil)
sometrue = var_unary_operation(MA.sometrue)
alltrue = var_unary_operation(MA.alltrue)
logical_not = var_unary_operation(MA.logical_not)

add = var_binary_operation(MA.add)
subtract = var_binary_operation(MA.subtract)
subtract.reduce = None
multiply = var_binary_operation(MA.multiply)
divide = var_binary_operation(MA.divide)
divide.reduce = None
remainder = var_binary_operation(MA.remainder)
remainder.reduce = None
fmod = var_binary_operation(MA.fmod)
fmod.reduce = None
hypot = var_binary_operation(MA.hypot)
hypot.reduce = None
arctan2 = var_binary_operation(MA.arctan2)
arctan2.reduce = None
equal = var_binary_operation(MA.equal)
equal.reduce = None
not_equal = var_binary_operation(MA.not_equal)
not_equal.reduce = None
less_equal = var_binary_operation(MA.less_equal)
less_equal.reduce = None
greater_equal = var_binary_operation(MA.greater_equal)
greater_equal.reduce = None
less = var_binary_operation(MA.less)
less.reduce = None
greater = var_binary_operation(MA.greater)
greater.reduce = None
logical_and = var_binary_operation(MA.logical_and)
logical_or = var_binary_operation(MA.logical_or)
logical_xor = var_binary_operation(MA.logical_xor)
bitwise_and = var_binary_operation(MA.bitwise_and)
bitwise_or = var_binary_operation(MA.bitwise_or)
bitwise_xor = var_binary_operation(MA.bitwise_xor)

def count (a, axis = None):
    "Count of the non-masked elements in a, or along a certain axis."   
    if axis is None:
        return MA.count(a,axis)
    else:
        ta = _makeMaskedArg(a)
        maresult = MA.count(ta,axis)
        axes, attributes, id, grid = _extractMetadata(a,omit=axis)
        return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)

def power (a, b, third=None):
    "a**b"
    ta = _makeMaskedArg(a)
    tb = _makeMaskedArg(b)
    maresult = MA.power(ta,tb,third)
    axes, attributes, id, grid = _extractMetadata(a)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)

def sum (a, axis = 0, fill_value=0):
    "Sum of elements along a certain axis."
    ta = _makeMaskedArg(a)
    maresult = MA.sum(ta, axis)
    axes, attributes, id, grid = _extractMetadata(a, omit=axis)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)

def product (a, axis = 0):
    "Product of elements along axis."
    ta = _makeMaskedArg(a)
    maresult = MA.product(ta, axis)
    axes, attributes, id, grid = _extractMetadata(a, omit=axis)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)

def average (a, axis=0, weights=None, returned=0):
    ta = _makeMaskedArg(a)
    maresult = MA.average(ta, axis, weights, returned)
    axes, attributes, id, grid = _extractMetadata(a, omit=axis)
    if returned: maresult, wresult = maresult
    r1 = TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)
    if returned:
        w1 = TransientVariable(wresult, axes=axes, grid=grid)
        return r1, w1
    else:
        return r1
average.__doc__ = MA.average.__doc__

def sort (a, axis=-1):
    ta = _makeMaskedArg(a)
    maresult = MA.sort(a, axis)
    axes, attributes, id, grid = _extractMetadata(a)
    sortaxis = axes[axis]
    if (grid is not None) and (sortaxis in grid.getAxisList()):
        grid = None
    axes[axis] = TransientAxis(Numeric.arange(len(sortaxis)))
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)
sort.__doc__ = MA.sort.__doc__ + "The sort axis is replaced with a dummy axis."

def choose (indices, t):
    """Returns an array shaped like indices containing elements chosen
      from t.
      If an element of t is the special element masked, any element
      of the result that "chooses" that element is masked.

      The result has only the default axes.
    """
    maresult = MA.choose(indices, map(_makeMaskedArg, t))
    return TransientVariable(maresult)

def where (condition, x, y):
    "where(condition, x, y) is x where condition is true, y otherwise" 
##    axes = commonDomain(x,y)
##    grid = commonGrid(x,y,axes)
    maresult = MA.where(condition, _makeMaskedArg(x), _makeMaskedArg(y))
    axes, attributes, id, grid = _extractMetadata(condition)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)

def masked_where(condition, x, copy=1):
    """Return x as an array masked where condition is true. 
       Also masked where x or condition masked.
    """
    tx = _makeMaskedArg(x)
    tcondition = _makeMaskedArg(condition)
    maresult = MA.masked_where(tcondition, tx, copy)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_greater(x, value):
    "masked_greater(x, value) = x masked where x > value"
    tx = _makeMaskedArg(x)
    maresult = MA.masked_greater(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_greater_equal(x, value):
    "masked_greater_equal(x, value) = x masked where x >= value"
    tx = _makeMaskedArg(x)
    maresult = MA.masked_greater_equal(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_less(x, value):
    "masked_less(x, value) = x masked where x < value"
    tx = _makeMaskedArg(x)
    maresult = MA.masked_less(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_less_equal(x, value):
    "masked_less_equal(x, value) = x masked where x <= value"
    tx = _makeMaskedArg(x)
    maresult = MA.masked_less_equal(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_not_equal(x, value):
    "masked_not_equal(x, value) = x masked where x != value"
    tx = _makeMaskedArg(x)
    maresult = MA.masked_not_equal(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_equal(x, value):
    """masked_equal(x, value) = x masked where x == value
       For floating point consider masked_values(x, value) instead.
    """
    tx = _makeMaskedArg(x)
    maresult = MA.masked_equal(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_outside(x, v1, v2):
    "x with mask of all values of x that are outside [v1,v2]"
    tx = _makeMaskedArg(x)
    maresult = MA.masked_outside(tx, v1, v2)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_inside(x, v1, v2):
    "x with mask of all values of x that are inside [v1,v2]"
    tx = _makeMaskedArg(x)
    maresult = MA.masked_inside(tx, v1, v2)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def concatenate (arrays, axis=0, axisid=None, axisattributes=None):
    """Concatenate the arrays along the given axis. Give the extended axis the id and
    attributes provided - by default, those of the first array."""
    tarrays = [_makeMaskedArg(a) for a in arrays]
    maresult = MA.concatenate(arrays, axis=axis)
    if len(arrays)>1:
        varattributes = None
        varid = None
        axes = commonDomain(tarrays[0],tarrays[1],omit=axis)
        grid = commonGrid(tarrays[0], tarrays[1], axes)
        for i in range(len(arrays)-2):
            if axes is None:
                break
            axes = commonAxes(tarrays[i+2],axes,omit=axis)
            grid = commonGrid1(a, grid, axes)
    else:
        axes = tarrays[0].getAxisList()
        varattributes = tarrays[0].attributes
        varid = tarrays[0].id
        if (isinstance(tarrays[0], TransientVariable)):
            grid = tarrays[0].getGrid()
        else:
            grid = None
    if axes is not None:
        if axisid is None:
            axisid = tarrays[0].getAxis(axis).id
        if axisattributes is None:
            axisattributes = tarrays[0].getAxis(0).attributes
        axes[axis] = axisConcatenate([t.getAxis(axis) for t in tarrays], axisid, axisattributes)

    # If the grid doesn't match the axislist (e.g., catenation was on latitude) then omit it.
    if grid is not None:
        for item in grid.getAxisList():
            if item not in axes:
                grid = None
    return TransientVariable(maresult, axes=axes, attributes=varattributes,id=varid,grid=grid)

def take (a, indices, axis=0):
    "take(a, indices, axis=0) returns selection of items from a."
    ta = _makeMaskedArg(a)
    maresult = MA.take(ta, indices, axis=axis)
    axes, attributes, id, grid = _extractMetadata(a)
    
    # If the take is on a grid axis, omit the grid.
    if (grid is not None) and (axes[axis] in grid.getAxisList()):
        grid = None
    if axes is not None:
        axes[axis] = axisTake(axes[axis], indices)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def transpose(a, axes=None):
    "transpose(a, axes=None) reorder dimensions per tuple axes"
    ta = _makeMaskedArg(a)
    tma = MA.masked_array(ta)
    if axes is None:
        axes = Numeric.arange(rank(tma))[::-1]
    maresult = MA.transpose(tma, axes=axes)
    oldaxes, attributes, id, grid = _extractMetadata(ta)
    newaxes = None
    if oldaxes is not None:
        newaxes = [oldaxes[i] for i in axes]
    return TransientVariable(maresult, axes=newaxes, attributes=attributes, id=id, grid=grid)

class _minimum_operation:
    "Object to calculate minima"
    def __init__ (self):
        """minimum(a, b) or minimum(a)
           In one argument case returns the scalar minimum.
        """
        pass

    def __call__ (self, a, b=None):
        "Execute the call behavior."
        a = _makeMaskedArg(a)
        if b is None:
            m = getmask(a)
            if m is None: 
                d = min(filled(a).flat)
                return d
            ac = a.compressed()
            if len(ac) == 0:
                return masked
            else:
                return min(ac.raw_data())
        else:
            return where(less(a, b), a, b)[...]
       
    def reduce (self, target, axis=0):
        """Reduce target along the given axis."""
        a = _makeMaskedArg(target)
        axes, attributes, id, grid = _extractMetadata(a, omit=axis)
        m = getmask(a)
        if m is None:
            t = filled(a)
            result = masked_array (Numeric.minimum.reduce (t, axis))
        else:
            t = Numeric.minimum.reduce(filled(a, MA.minimum_fill_value(a)), axis)
            m = Numeric.logical_and.reduce(m, axis)
            result = masked_array(t, m, MA.fill_value(a))
        return TransientVariable(result, axes=axes, copy=0,
                     fill_value=MA.fill_value(a), grid=grid)

    def outer (self, a, b):
        "Return the function applied to the outer product of a and b."
        a = _makeMaskedArg(a)
        b = _makeMaskedArg(b)
        ma = getmask(a)
        mb = getmask(b)
        if ma is None and mb is None:
            m = None
        else:
            ma = getmaskarray(a)
            mb = getmaskarray(b)
            m = logical_or.outer(ma, mb)
        d = Numeric.minimum.outer(filled(a), filled(b))
        return TransientVariable(d, mask=m, copy=0)

minimum = _minimum_operation () 
    
class _maximum_operation:
    "Object to calculate maxima"
    def __init__ (self):
        """maximum(a, b) or maximum(a)
           In one argument case returns the scalar maximum.
        """
        pass

    def __call__ (self, a, b=None):
        "Execute the call behavior."
        a = _makeMaskedArg(a)
        if b is None:
            m = getmask(a)
            if m is None: 
                d = max(filled(a).flat)
                return d
            ac = a.compressed()
            if len(ac) == 0:
                return masked
            else:
                return max(ac.raw_data())
        else:
            return where(greater(a, b), a, b)[...]
       
    def reduce (self, target, axis=0):
        """Reduce target along the given axis."""
        axes, attributes, id, grid = _extractMetadata(target, omit=axis)
        a = _makeMaskedArg(target)
        m = getmask(a)
        if m is None:
            t = filled(a)
            return masked_array (Numeric.maximum.reduce (t, axis))
        else:
            t = Numeric.maximum.reduce(filled(a, MA.maximum_fill_value(a)), axis)
            m = Numeric.logical_and.reduce(m, axis)
            return TransientVariable(t, mask=m, fill_value=MA.fill_value(a),
                        axes = axes, grid=grid)

    def outer (self, a, b):
        "Return the function applied to the outer product of a and b."
        a = _makeMaskedArg(a)
        b = _makeMaskedArg(b)
        ma = getmask(a)
        mb = getmask(b)
        if ma is None and mb is None:
            m = None
        else:
            ma = getmaskarray(a)
            mb = getmaskarray(b)
            m = logical_or.outer(ma, mb)
        d = Numeric.maximum.outer(filled(a), filled(b))
        return TransientVariable(d, mask=m)

maximum = _maximum_operation () 
    
def asarray(data, typecode=None):
    """asarray(data, typecode=None) = array(data, typecode=None, copy=0)
       Returns data if typecode if data is a MaskedArray and typecode None
       or the same.
    """
    if isinstance(data, AbstractVariable) and (typecode is None or typecode == data.typecode()):
        return data
    else:
        return TransientVariable(data, typecode=typecode, copy=0)

def arrayrange(start, stop=None, step=1, typecode=None, axis=None, attributes=None, id=None):
    """Just like range() except it returns a variable whose type can be specfied
    by the keyword argument typecode. The axis of the result variable may be specified.
    """
    maresult = MA.arrayrange(start, stop=stop, step=step, typecode=typecode)
    return TransientVariable(maresult, axes=(axis,), attributes=attributes, id=id)

arange = arrayrange

def zeros (shape, typecode=Int, savespace=0, axes=None, attributes=None, id=None, grid=None):
    """zeros(n, typecode=Int, savespace=0, axes=None, attributes=None, id=None) = 
     an array of all zeros of the given length or shape."""
    maresult = MA.zeros(shape, typecode=typecode, savespace=savespace)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)
    
def ones (shape, typecode=Int, savespace=0, axes=None, attributes=None, id=None, grid=None):
    """ones(n, typecode=Int, savespace=0, axes=None, attributes=None, id=None) = 
     an array of all ones of the given length or shape."""
    maresult = MA.ones(shape, typecode=typecode, savespace=savespace)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

as_masked = MA.array

def outerproduct(a, b):
    """outerproduct(a,b) = {a[i]*b[j]}, has shape (len(a),len(b))"""
    ta = asVariable(a,writeable=1)
    tb = asVariable(b,writeable=1)
    maresult = MA.outerproduct(ta,tb)
    axes = (ta.getAxis(0),tb.getAxis(0))
    return TransientVariable(maresult, axes=axes)

def argsort (x, axis = -1, fill_value=None):
    """Treating masked values as if they have the value fill_value,
       return sort indices for sorting along given axis.
       if fill_value is None, use fill_value(x)
    """        
    tx = _makeMaskedArg(x)
    maresult = MA.argsort(tx,axis=axis,fill_value=fill_value)
    axes, attributes, id, grid = _extractMetadata(x)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

array = TransientVariable

def repeat(a, repeats, axis=0):
    """repeat elements of a repeats times along axis
       repeats is a sequence of length a.shape[axis]
       telling how many times to repeat each element.
    """
    ta = _makeMaskedArg(a)
    maresult = MA.repeat(ta, repeats, axis=axis)
    axes, attributes, id, grid = _extractMetadata(a)
    if (grid is not None) and (axes[axis] in grid.getAxisList()):
        grid = None
    if axes is not None:
        axes[axis] = None
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def reshape (a, newshape, axes=None, attributes=None, id=None, grid=None):
    "Copy of a with a new shape."
    ignore, attributes, id, ignore = _extractMetadata(a, axes, attributes, id)
    if axes is not None:
        axesshape = [len(item) for item in axes]
        if axesshape!=list(newshape):
            raise CDMSError, 'axes must be shaped %s'%`newshape`
    ta = _makeMaskedArg(a)
    maresult = MA.reshape(ta, newshape)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def left_shift (a, n):
    "Left shift n bits"
    ta = _makeMaskedArg(a)
    tb = _makeMaskedArg(n)
    maresult = MA.left_shift(ta,MA.filled(tb))
    axes, attributes, id, grid = _extractMetadata(a)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)

def right_shift (a, n):
    "Right shift n bits"
    ta = _makeMaskedArg(a)
    tb = _makeMaskedArg(n)
    maresult = MA.right_shift(ta,MA.filled(tb))
    axes, attributes, id, grid = _extractMetadata(a)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid)

def resize (a, new_shape, axes=None, attributes=None, id=None, grid=None):
    """resize(a, new_shape) returns a new array with the specified shape.
    The original array's total size can be any size."""
    ignore, attributes, id, ignore = _extractMetadata(a, axes, attributes, id)
    if axes is not None:
        axesshape = [len(item) for item in axes]
        if axesshape!=list(new_shape):
            raise CDMSError, 'axes must be shaped %s'%`newshape`
    ta = _makeMaskedArg(a)
    maresult = MA.resize(ta, new_shape)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_array (a, mask=None, fill_value=None, axes=None, attributes=None, id=None):
    """masked_array(a, mask=None) = 
       array(a, mask=mask, copy=0, fill_value=fill_value)
       Use fill_value(a) if None.
    """
    maresult = MA.masked_array(_makeMaskedArg(a), mask=mask, fill_value=fill_value)
    axes, attributes, id, grid = _extractMetadata(a, axes, attributes, id)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_values (data, value, rtol=1.e-5, atol=1.e-8, copy=1,
    savespace=0, axes=None, attributes=None, id=None): 
    """
       masked_values(data, value, rtol=1.e-5, atol=1.e-8)
       Create a masked array; mask is None if possible.
       May share data values with original array, but not recommended.
       Masked where abs(data-value)<= atol + rtol * abs(value)
    """
    maresult = MA.masked_values(_makeMaskedArg(data), value, rtol=rtol, atol=atol, copy=copy, savespace=savespace)
    axes, attributes, id, grid = _extractMetadata(data, axes, attributes, id)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

def masked_object (data, value, copy=1, savespace=0, axes=None, attributes=None, id=None):
    "Create array masked where exactly data equal to value"
    maresult = MA.masked_object(_makeMaskedArg(data), value, copy=copy, savespace=savespace)
    axes, attributes, id, grid = _extractMetadata(data, axes, attributes, id)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)
    
def isMaskedVariable (x):
    "Is x a masked variable, that is, an instance of AbstractVariable?"
    return isinstance(x, AbstractVariable)

def set_default_fill_value(value_type, value):
    """Set the default fill value for value_type to value.
    value_type is a string: 'real','complex','character','integer',or 'object'.
    value should be a scalar or single-element array.
    """
    if value_type == 'real':
        MA.default_real_fill_value = value
    elif value_type == 'integer':
        MA.default_integer_fill_value = value
    elif value_type == 'complex':
        MA.default_complex_fill_value = value
    elif value_type == 'character':
        MA.default_character_fill_value = value
    elif value_type == 'object':
        MA.default_object_fill_value = value

def fromfunction (f, dimensions):
    """Apply f to s to create an array as in Numeric."""
    return TransientVariable(MA.fromfunction(f, dimensions))

def diagonal (a, offset = 0, axis1=0, axis2 = 1):
    """diagonal(a, offset=0, axis1=0, axis2 = 1) returns the given 
       diagonals defined by the two dimensions of the array.
    """
    from MA.MA_version import version_info
    if version_info > (9,0,0,'final',0):
        return TransientVariable(MA.diagonal(_makeMaskedArg(a), 
                offset, axis1, axis2))
    elif axis1==0 and axis2 ==1:
        return TransientVariable(MA.diagonal(_makeMaskedArg(a), offset))
    else:
        raise RuntimeError, 'Four argument version of diagonal not in MA'

def fromstring (s, t):
    """Construct a masked array from a string. Result will have no mask.
       t is a typecode.
    """
    return TransientArray(MA.fromstring(s,t))




