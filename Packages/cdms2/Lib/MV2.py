## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"CDMS Variable objects, MaskedArray interface"
import numpy
import typeconv
from numpy import character, float, float32, float64, int, int8, int16, int32
from numpy.ma import allclose, allequal, common_fill_value, compress, make_mask_none, dot, filled, \
     getmask, getmaskarray, identity, indices, innerproduct, masked, put, putmask, rank, ravel, \
     set_fill_value, shape, size, isMA, isMaskedArray, is_mask, isarray, \
     make_mask, make_mask_none, mask_or, nomask
from numpy import sctype2char, get_printoptions, set_printoptions
from avariable import AbstractVariable, getNumericCompatibility
from tvariable import TransientVariable, asVariable
from grid import AbstractRectGrid
from error import CDMSError
#from numpy.ma import *
from axis import allclose as axisAllclose, TransientAxis, concatenate as axisConcatenate, take as axisTake



create_mask = make_mask_none
e = numpy.e
pi = numpy.pi
#NewAxis = numpy.oldnumeric.NewAxis
newaxis = numpy.newaxis
counter = 0

def fill_value(ar):
    return ar.fill_value


def _makeMaskedArg(x):
    """If x is a variable, turn it into a TransientVariable."""
    if isinstance(x, AbstractVariable) and not isinstance(x, TransientVariable):
        return x.subSlice()
    elif isinstance(x,TransientVariable):
        return x
    else:
        return array(x)


def _extractMetadata(a, axes=None, attributes=None, id=None, omit=None, omitall=False):
    """Extract axes, attributes, id from 'a', if arg is None."""
    resultgrid = None
    if isinstance(a, AbstractVariable):
        if axes is None:
            axes = a.getAxisList(omit=omit)
        if omitall:
            axes = None
        if attributes is None:
            attributes = a.attributes
        if id is None:
            id = "variable_%i" % TransientVariable.variable_count
            TransientVariable.variable_count+=1

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
        mafunc is an numpy.ma masked_unary_function.
        """
        self.mafunc = mafunc
        self.__doc__ = mafunc.__doc__

    def __call__ (self, a):
        axes, attributes, id, grid = _extractMetadata(a)
        maresult = self.mafunc(_makeMaskedArg(a))
        return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

class var_unary_operation_with_axis:
    def __init__(self, mafunc):
        """ var_unary_operation(mafunc)
        mafunc is an numpy.ma masked_unary_function.
        """
        self.mafunc = mafunc
        self.__doc__ = mafunc.__doc__
    def __call__ (self, a, axis=0):
        axis = _conv_axis_arg(axis)
        ta = _makeMaskedArg(a)
        maresult = self.mafunc(ta, axis=axis)
        axes, attributes, id, grid = _extractMetadata(a, omit=axis, omitall=(axis is None))
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
        if arank>brank:
            maxrank = arank
            minrank = brank
        else:
            maxrank = brank
            minrank = arank
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
                common[j] = TransientAxis(numpy.arange(len(aj)))

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
    is returned. a and b can be numpy arrays, in which case the result is None.

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
        mafunc is an numpy.ma masked_binary_function.
        """
        self.mafunc = mafunc
        self.__doc__ = mafunc.__doc__

    def __call__ (self, a, b):
        id = "variable_%i" % TransientVariable.variable_count
        TransientVariable.variable_count+=1
        axes = commonDomain(a,b)
        grid = commonGrid(a,b,axes)
        ta = _makeMaskedArg(a)
        tb = _makeMaskedArg(b)
        maresult = self.mafunc(ta,tb)
        return TransientVariable(maresult, axes=axes, grid=grid,no_update_from=True,id=id)

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


def compress(a,b):
   __doc__=numpy.ma.__doc__
   import warnings
   warnings.warn("arguments order for compress function has changed\nit is now: MV2.copmress(array,condition), if your code seems to not react or act wrong to a call to compress, please check this", Warning)
   return TransientVariable(numpy.ma.compress(a,b),copy=1)


sqrt = var_unary_operation(numpy.ma.sqrt)
absolute = var_unary_operation(numpy.ma.absolute)
negative = var_unary_operation(numpy.ma.negative)
not_equal = var_binary_operation(numpy.ma.not_equal)

add = var_binary_operation(numpy.ma.add)
subtract = var_binary_operation(numpy.ma.subtract)
multiply = var_binary_operation(numpy.ma.multiply)
divide = var_binary_operation(numpy.ma.divide)
equal = var_binary_operation(numpy.ma.equal)
less_equal = var_binary_operation(numpy.ma.less_equal)
greater_equal = var_binary_operation(numpy.ma.greater_equal)
less = var_binary_operation(numpy.ma.less)
greater = var_binary_operation(numpy.ma.greater)
def power (a, b, third=None):
    "a**b"
    ta = _makeMaskedArg(a)
    tb = _makeMaskedArg(b)
    maresult = numpy.ma.power(ta,tb,third)
    axes, attributes, id, grid = _extractMetadata(a)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id)

def left_shift (a, n):
    "Left shift n bits"
    ta = _makeMaskedArg(a)
    tb = _makeMaskedArg(n)
    maresult = numpy.ma.left_shift(ta,numpy.ma.filled(tb))
    axes, attributes, id, grid = _extractMetadata(a)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id)

def right_shift (a, n):
    "Right shift n bits"
    ta = _makeMaskedArg(a)
    tb = _makeMaskedArg(n)
    maresult = numpy.ma.right_shift(ta,numpy.ma.filled(tb))
    axes, attributes, id, grid = _extractMetadata(a)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id)


def _convdtype(dtype, typecode):
    "Resolve dtype, typecode args"
    if dtype is None and typecode is not None:
        dtype = typeconv.convtypecode2(typecode)
    return dtype

def _conv_axis_arg(axis):
    "Handle backward compatibility with numpy for axis arg"
    if getNumericCompatibility() and axis is None:
        axis=0
    return axis

def is_masked(x):
    "Is x a 0-D masked value?"
    return isMaskedArray(x) and x.size==1 and x.ndim==0 and x.mask.item()

def is_floating(x):
    "Is x a scalar float, either python or numpy?"
    return (isinstance(x, numpy.floating) or isinstance(x, float))

def is_integer(x):
    "Is x a scalar integer, either python or numpy?"
    return (isinstance(x, numpy.integer) or isinstance(x, int) or isinstance(x, long))

def get_print_limit():
    return get_printoptions()['threshold']

def set_print_limit(limit=numpy.inf):
    set_printoptions(threshold=limit)

subtract.reduce = None
log = var_unary_operation(numpy.ma.log)
log10 = var_unary_operation(numpy.ma.log10)
exp = var_unary_operation(numpy.ma.exp)
conjugate = var_unary_operation(numpy.ma.conjugate)
sin = var_unary_operation(numpy.ma.sin)
cos = var_unary_operation(numpy.ma.cos)
tan = var_unary_operation(numpy.ma.tan)
arcsin = var_unary_operation(numpy.ma.arcsin)
arccos = var_unary_operation(numpy.ma.arccos)
arctan = var_unary_operation(numpy.ma.arctan)
sinh = var_unary_operation(numpy.ma.sinh)
cosh = var_unary_operation(numpy.ma.cosh)
tanh = var_unary_operation(numpy.ma.tanh)
fabs = var_unary_operation(numpy.ma.fabs)
nonzero = var_unary_operation(numpy.ma.nonzero)
around = var_unary_operation(numpy.ma.around)
floor = var_unary_operation(numpy.ma.floor)
ceil = var_unary_operation(numpy.ma.ceil)
sometrue = var_unary_operation_with_axis(numpy.ma.sometrue)
alltrue = var_unary_operation_with_axis(numpy.ma.alltrue)
logical_not = var_unary_operation(numpy.ma.logical_not)
divide.reduce = None
remainder = var_binary_operation(numpy.ma.remainder)
remainder.reduce = None
fmod = var_binary_operation(numpy.ma.fmod)
fmod.reduce = None
hypot = var_binary_operation(numpy.ma.hypot)
hypot.reduce = None
arctan2 = var_binary_operation(numpy.ma.arctan2)
arctan2.reduce = None
less.reduce = None
equal.reduce = None
not_equal.reduce = None
less_equal.reduce = None
greater_equal.reduce = None
greater.reduce = None
logical_and = var_binary_operation(numpy.ma.logical_and)
logical_or = var_binary_operation(numpy.ma.logical_or)
logical_xor = var_binary_operation(numpy.ma.logical_xor)
bitwise_and = var_binary_operation(numpy.ma.bitwise_and)
bitwise_or = var_binary_operation(numpy.ma.bitwise_or)
bitwise_xor = var_binary_operation(numpy.ma.bitwise_xor)


def count (a, axis = None):
    "Count of the non-masked elements in a, or along a certain axis."   
    if axis is None:
        return numpy.ma.count(a,axis)
    else:
        ta = _makeMaskedArg(a)
        maresult = numpy.ma.count(ta,axis)
        axes, attributes, id, grid = _extractMetadata(a,omit=axis)
        F=getattr(a,"fill_value",1.e20)
        return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id, fill_value=F)

def sum (a, axis = None, fill_value=0, dtype=None):
    "Sum of elements along a certain axis."
    axis = _conv_axis_arg(axis)
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.sum(ta, axis, dtype=dtype)
    axes, attributes, id, grid = _extractMetadata(a, omit=axis, omitall=(axis is None))
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id, fill_value=F)

def product (a, axis = 0, dtype=None):
    "Product of elements along axis."
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.product(ta, axis, dtype=dtype)
    axes, attributes, id, grid = _extractMetadata(a, omit=axis)
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id, fill_value=F)

def average (a, axis=None, weights=None, returned=False):
    axis = _conv_axis_arg(axis)
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.average(ta, axis, weights, returned)
    axes, attributes, id, grid = _extractMetadata(a, omit=axis, omitall=(axis is None))
    if returned: maresult, wresult = maresult
    F=getattr(a,"fill_value",1.e20)
    r1 = TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id,no_update_from=True, fill_value=F)
    if returned:
        F=getattr(a,"fill_value",1.e20)
        w1 = TransientVariable(wresult, axes=axes, grid=grid, id=id,no_update_from=True, fill_value=F)
        return r1, w1
    else:
        return r1
average.__doc__ = numpy.ma.average.__doc__

def max (a, axis=None):
    axis = _conv_axis_arg(axis)
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.max(ta, axis)
    axes, attributes, id, grid = _extractMetadata(a, omit=axis, omitall=(axis is None))
    F=getattr(a,"fill_value",1.e20)
    r1 = TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id,no_update_from=True, fill_value=F)
    return r1
max.__doc__ = numpy.ma.max.__doc__
def min (a, axis=None):
    axis = _conv_axis_arg(axis)
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.min(ta, axis)
    axes, attributes, id, grid = _extractMetadata(a, omit=axis, omitall=(axis is None))
    F=getattr(a,"fill_value",1.e20)
    r1 = TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id,no_update_from=True, fill_value=F)
    return r1
min.__doc__ = numpy.ma.min.__doc__

def sort (a, axis=-1):
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.sort(a.asma(), axis)
    axes, attributes, id, grid = _extractMetadata(a)
    sortaxis = axes[axis]
    if (grid is not None) and (sortaxis in grid.getAxisList()):
        grid = None
    axes[axis] = TransientAxis(numpy.arange(len(sortaxis)))
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id, fill_value=F)
sort.__doc__ = numpy.ma.sort.__doc__ + "The sort axis is replaced with a dummy axis."

def choose (indices, t):
    """Returns an array shaped like indices containing elements chosen
      from t.
      If an element of t is the special element masked, any element
      of the result that "chooses" that element is masked.

      The result has only the default axes.
    """
    maresult = numpy.ma.choose(indices, map(_makeMaskedArg, t))
    F=getattr(t,"fill_value",1.e20)
    return TransientVariable(maresult, fill_value=F)

def where (condition, x, y):
    "where(condition, x, y) is x where condition is true, y otherwise" 
##    axes = commonDomain(x,y)
##    grid = commonGrid(x,y,axes)
    maresult = numpy.ma.where(condition, _makeMaskedArg(x), _makeMaskedArg(y))
    axes, attributes, id, grid = _extractMetadata(condition)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, grid=grid, id=id, fill_value=F)

def masked_where(condition, x, copy=1):
    """Return x as an array masked where condition is true. 
       Also masked where x or condition masked.
    """
    tx = _makeMaskedArg(x)
    tcondition = _makeMaskedArg(condition)
    maresult = numpy.ma.masked_where(tcondition, tx, copy)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_greater(x, value):
    "masked_greater(x, value) = x masked where x > value"
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.masked_greater(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)


def masked_greater_equal(x, value):
    "masked_greater_equal(x, value) = x masked where x >= value"
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.masked_greater_equal(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_less(x, value):
    "masked_less(x, value) = x masked where x < value"
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.masked_less(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_less_equal(x, value):
    "masked_less_equal(x, value) = x masked where x <= value"
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.masked_less_equal(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_not_equal(x, value):
    "masked_not_equal(x, value) = x masked where x != value"
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.masked_not_equal(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_equal(x, value):
    """masked_equal(x, value) = x masked where x == value
       For floating point consider masked_values(x, value) instead.
    """
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.masked_equal(tx, value)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_outside(x, v1, v2):
    "x with mask of all values of x that are outside [v1,v2]"
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.masked_outside(tx, v1, v2)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_inside(x, v1, v2):
    "x with mask of all values of x that are inside [v1,v2]"
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.masked_inside(tx, v1, v2)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def concatenate (arrays, axis=0, axisid=None, axisattributes=None):
    """Concatenate the arrays along the given axis. Give the extended axis the id and
    attributes provided - by default, those of the first array."""

    tarrays = [_makeMaskedArg(a) for a in arrays]
    maresult = numpy.ma.concatenate(arrays, axis=axis)
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
        allunitsequal=True
        try:
            allunits=tarrays[0].getAxis(axis).units
        except:
            allunits=None
        for t in tarrays[1:]:
            try:
                tunits=t.getAxis(axis).units
            except:
                tunits=None
            if tunits!=allunits:
                allunitsequal=False
        if allunitsequal:
            if axisattributes is None:
                axisattributes = tarrays[0].getAxis(axis).attributes
            axes[axis] = axisConcatenate([t.getAxis(axis) for t in tarrays], axisid, axisattributes)

    # If the grid doesn't match the axislist (e.g., catenation was on latitude) then omit it.
    if grid is not None:
        for item in grid.getAxisList():
            if item not in axes:
                grid = None
    F=getattr(arrays[0],"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=varattributes,id=varid,grid=grid, fill_value=F)

def take (a, indices, axis=None):
    "take(a, indices, axis=None) returns selection of items from a."
    axis = _conv_axis_arg(axis)
    ta = _makeMaskedArg(a)

    # ma compatibility interface has a bug
    maresult = numpy.ma.take(ta, indices, axis=axis)
    axes, attributes, id, grid = _extractMetadata(a, omitall=(axis is None))
    
    # If the take is on a grid axis, omit the grid.
    if (grid is not None) and (axes[axis] in grid.getAxisList()):
        grid = None
    if axes is not None:
        axes[axis] = axisTake(axes[axis], indices)
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def transpose(a, axes=None):
    "transpose(a, axes=None) reorder dimensions per tuple axes"
    ta = _makeMaskedArg(a)
    tma = numpy.ma.masked_array(ta)
    if axes is None:
        axes = numpy.arange(tma.ndim)[::-1]
    maresult = numpy.ma.transpose(tma, axes=axes)
    oldaxes, attributes, id, grid = _extractMetadata(ta)
    newaxes = None
    if oldaxes is not None:
        newaxes = [oldaxes[i] for i in axes]
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=newaxes, attributes=attributes, id=id, grid=grid, copy=1, fill_value=F)

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
            if m is nomask: 
                d = numpy.min(filled(a).ravel())
                return d
##             ac = a.compressed()
##             if len(ac) == 0:
##                 return masked
            else:
                return numpy.ma.min(a)
        else:
            return where(less(a, b), a, b)[...]
       
    def reduce (self, target, axis=0):
        """Reduce target along the given axis."""
        a = _makeMaskedArg(target)
        axes, attributes, id, grid = _extractMetadata(a, omit=axis)
        m = getmask(a)
        if m is nomask:
            t = filled(a)
            result = masked_array (numpy.minimum.reduce (t, axis))
        else:
            t = numpy.minimum.reduce(filled(a, numpy.ma.minimum_fill_value(a)), axis)
            m = numpy.logical_and.reduce(m, axis)
            result = masked_array(t, m, fill_value(a))
        return TransientVariable(result, axes=axes, copy=0,
                     fill_value=fill_value(a), grid=grid, id=id)

    def outer (self, a, b):
        "Return the function applied to the outer product of a and b."
        a = _makeMaskedArg(a)
        b = _makeMaskedArg(b)
        ma = getmask(a)
        mb = getmask(b)
        if ma is nomask and mb is nomask:
            m = None
        else:
            ma = getmaskarray(a)
            mb = getmaskarray(b)
            m = logical_or.outer(ma, mb)
        d = numpy.minimum.outer(filled(a), filled(b))
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
            if m is nomask: 
                d = numpy.max(filled(a).ravel())
                return d
##             ac = a.compressed()
##             if len(ac) == 0:
##                 return masked
            else:
                return numpy.ma.max(a)
        else:
            return where(greater(a, b), a, b)[...]
       
    def reduce (self, target, axis=0):
        """Reduce target along the given axis."""
        axes, attributes, id, grid = _extractMetadata(target, omit=axis)
        a = _makeMaskedArg(target)
        m = getmask(a)
        if m is nomask:
            t = filled(a)
            return masked_array (numpy.maximum.reduce (t, axis))
        else:
            t = numpy.maximum.reduce(filled(a, numpy.ma.maximum_fill_value(a)), axis)
            m = numpy.logical_and.reduce(m, axis)
            return TransientVariable(t, mask=m, fill_value=fill_value(a),
                        axes = axes, grid=grid, id=id)

    def outer (self, a, b):
        "Return the function applied to the outer product of a and b."
        a = _makeMaskedArg(a)
        b = _makeMaskedArg(b)
        ma = getmask(a)
        mb = getmask(b)
        if ma is nomask and mb is nomask:
            m = None
        else:
            ma = getmaskarray(a)
            mb = getmaskarray(b)
            m = logical_or.outer(ma, mb)
        d = numpy.maximum.outer(filled(a), filled(b))
        return TransientVariable(d, mask=m)

maximum = _maximum_operation () 
    
def asarray(data, typecode=None, dtype=None):
    """asarray(data, typecode=None, dtype=None) is equivalent to array(data, dtype=None, copy=0)
       Returns data if dtype is None or data is a MaskedArray of the same dtype.
       typecode arg is for backward compatibility.
    """
    dtype = _convdtype(dtype, typecode)
    if isinstance(data, AbstractVariable) and (dtype is None or sctype2char(dtype) == data.dtype.char):
        return data
    else:
        F=getattr(data,"fill_value",1.e20)
        return TransientVariable(data, dtype=dtype, copy=0, fill_value=F)

def arrayrange(start, stop=None, step=1, typecode=None, axis=None, attributes=None, id=None, dtype=None):
    """Just like range() except it returns a variable whose type can be specfied
    by the keyword argument typecode. The axis of the result variable may be specified.
    """
    dtype = _convdtype(dtype, typecode)
    if stop is None:
        maresult = numpy.ma.arange(start, step=step, dtype=dtype)
    else:
        maresult = numpy.ma.arange(start, stop=stop, step=step, dtype=dtype)
    return TransientVariable(maresult, axes=(axis,), attributes=attributes, id=id)

arange = arrayrange

def zeros (shape, typecode=float, savespace=0, axes=None, attributes=None, id=None, grid=None, dtype=None):
    """zeros(n, typecode=float, savespace=0, axes=None, attributes=None, id=None) = 
     an array of all zeros of the given length or shape."""
    dtype = _convdtype(dtype, typecode)
    maresult = numpy.ma.zeros(shape, dtype=dtype)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)
    
def ones (shape, typecode=float, savespace=0, axes=None, attributes=None, id=None, grid=None, dtype=None):
    """ones(n, typecode=float, savespace=0, axes=None, attributes=None, id=None) = 
     an array of all ones of the given length or shape."""
    dtype = _convdtype(dtype, typecode)
    maresult = numpy.ma.ones(shape, dtype=dtype)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid)

as_masked = numpy.ma.array

def outerproduct(a, b):
    """outerproduct(a,b) = {a[i]*b[j]}, has shape (len(a),len(b))"""
    ta = asVariable(a,writeable=1)
    tb = asVariable(b,writeable=1)
    maresult = numpy.ma.outerproduct(ta,tb)
    axes = (ta.getAxis(0),tb.getAxis(0))
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, fill_value=F)

def argsort (x, axis = -1, fill_value=None):
    """Treating masked values as if they have the value fill_value,
       return sort indices for sorting along given axis.
       if fill_value is None, use fill_value(x)
    """        
    tx = _makeMaskedArg(x)
    maresult = numpy.ma.argsort(tx,axis=axis,fill_value=fill_value)
    axes, attributes, id, grid = _extractMetadata(x)
    F=getattr(x,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

array = TransientVariable

def repeat(a, repeats, axis=None):
    """repeat elements of a repeats times along axis
       repeats is a sequence of length a.shape[axis]
       telling how many times to repeat each element.
    """
    axis = _conv_axis_arg(axis)
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.repeat(ta, repeats, axis=axis)
    axes, attributes, id, grid = _extractMetadata(a, omitall=(axis is None))
    if (grid is not None) and (axes[axis] in grid.getAxisList()):
        grid = None
    if axes is not None:
        axes[axis] = None
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, no_update_from=True, fill_value=F)

def reshape (a, newshape, axes=None, attributes=None, id=None, grid=None):
    ignore, attributes, id, ignore = _extractMetadata(a, axes, attributes, id)
    if axes is not None:
        axesshape = [len(item) for item in axes]
        if axesshape!=list(newshape):
            raise CDMSError, 'axes must be shaped %s'%`newshape`
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.reshape(ta, newshape)
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, no_update_from=True, fill_value=F)
reshape.__doc__="numpy doc: %s\naxes/attributes/grid are applied onto the new variable" % numpy.reshape.__doc__

def resize (a, new_shape, axes=None, attributes=None, id=None, grid=None):
    """resize(a, new_shape) returns a new array with the specified shape.
    The original array's total size can be any size."""
    ignore, attributes, id, ignore = _extractMetadata(a, axes, attributes, id)
    if axes is not None:
        axesshape = [len(item) for item in axes]
        if axesshape!=list(new_shape):
            raise CDMSError, 'axes must be shaped %s'%`newshape`
    ta = _makeMaskedArg(a)
    maresult = numpy.ma.resize(ta, new_shape)
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_array (a, mask=None, fill_value=None, axes=None, attributes=None, id=None):
    """masked_array(a, mask=None) = 
       array(a, mask=mask, copy=0, fill_value=fill_value)
       Use fill_value(a) if None.
    """
    maresult = numpy.ma.masked_array(_makeMaskedArg(a), mask=mask, fill_value=fill_value)
    axes, attributes, id, grid = _extractMetadata(a, axes, attributes, id)
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_values (data, value, rtol=1.e-5, atol=1.e-8, copy=1,
    savespace=0, axes=None, attributes=None, id=None): 
    """
       masked_values(data, value, rtol=1.e-5, atol=1.e-8)
       Create a masked array; mask is None if possible.
       May share data values with original array, but not recommended.
       Masked where abs(data-value)<= atol + rtol * abs(value)
    """
    maresult = numpy.ma.masked_values(_makeMaskedArg(data), value, rtol=rtol, atol=atol, copy=copy)
    axes, attributes, id, grid = _extractMetadata(data, axes, attributes, id)
    F=getattr(data,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)

def masked_object (data, value, copy=1, savespace=0, axes=None, attributes=None, id=None):
    "Create array masked where exactly data equal to value"
    maresult = numpy.ma.masked_object(_makeMaskedArg(data), value, copy=copy)
    axes, attributes, id, grid = _extractMetadata(data, axes, attributes, id)
    F=getattr(data,"fill_value",1.e20)
    return TransientVariable(maresult, axes=axes, attributes=attributes, id=id, grid=grid, fill_value=F)
    
def isMaskedVariable (x):
    "Is x a masked variable, that is, an instance of AbstractVariable?"
    return isinstance(x, AbstractVariable)

def set_default_fill_value(value_type, value):
    """Set the default fill value for value_type to value.
    value_type is a string: 'real','complex','character','integer',or 'object'.
    value should be a scalar or single-element array.
    """
    if value_type == 'real':
        numpy.ma.default_real_fill_value = value
    elif value_type == 'integer':
        numpy.ma.default_integer_fill_value = value
    elif value_type == 'complex':
        numpy.ma.default_complex_fill_value = value
    elif value_type == 'character':
        numpy.ma.default_character_fill_value = value
    elif value_type == 'object':
        numpy.ma.default_object_fill_value = value

def fromfunction (f, dimensions):
    """Apply f to s to create an array as in numpy."""
    return TransientVariable(numpy.ma.fromfunction(f, dimensions))

def diagonal (a, offset = 0, axis1=0, axis2 = 1):
    """diagonal(a, offset=0, axis1=0, axis2 = 1) returns the given 
       diagonals defined by the two dimensions of the array.
    """
    F=getattr(a,"fill_value",1.e20)
    return TransientVariable(numpy.ma.diagonal(_makeMaskedArg(a), 
            offset, axis1, axis2), fill_value=F)

def fromstring (s, t):
    """Construct a masked array from a string. Result will have no mask.
       t is a typecode.
    """
    return TransientArray(numpy.ma.fromstring(s,t))




