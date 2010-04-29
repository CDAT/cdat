"""Reference implementation of PEP 0242"""
import PropertiedClasses
import _kinds

class KindObject (PropertiedClasses.PropertiedClass):
    """A KindObject is a constructor for certain varieties of integers,
     floating point numbers, and complex numbers.
    """
    def __init__ (self, **meta):
        for k, v in meta.items():
            self._basic_set(k, v)
            self.set_property(k, nowrite=1)

    def str (self):
        return '<KindObject named ' + self.name + '>'

class IntKindObject (KindObject):
    "Kind object for standard integers, corresponding to C ints."
    def __init__(self):
        "Create the kind object for standard integers."
        KindObject.__init__(self, 
                         name='int', 
                         typecode='i', 
                         MAX=_kinds.INT_MAX,
                         MIN=_kinds.INT_MIN
                         )
    def __call__ (self, n):
        "Convert to a standard integer."
        try:
            return int(n)
        except Exception, e:
            raise OverflowError, 'Could not convert to standard integer.'

class LongKindObject (KindObject):
    "Kind object for Python long -- no corresponding C type."
    def __init__(self):
        "Create the kind object for standard integers."
        KindObject.__init__(self, 
                         name='long', 
                         typecode='L', 
                         MAX=None,
                         MIN=None
                         )
    def __call__ (self, n):
        "Convert to a standard integer."
        try:
            return long(n)
        except Exception, e:
            raise OverflowError, 'Could not convert to standard integer.'

class DoubleKindObject (KindObject):
    "Kind object for a floating type corresponding to C double."
    def __init__ (self):
        """Create the kind object corresponding to a C double, standard Python real."""
        KindObject.__init__ (self,
                         name='double', 
                         typecode='d',
                         MAX=_kinds.DBL_MAX, 
                         MIN=_kinds.DBL_MIN,
                         DIG=_kinds.DBL_DIG,
                         EPSILON=_kinds.DBL_EPSILON,
                         MAX_EXP=_kinds.DBL_MAX_EXP,
                         MIN_EXP=_kinds.DBL_MIN_EXP,
                         MAX_10_EXP=_kinds.DBL_MAX_10_EXP,
                         MIN_10_EXP=_kinds.DBL_MIN_10_EXP,
                         RADIX=_kinds.FLT_RADIX,
                         ROUNDS=_kinds.FLT_ROUNDS)

    def __call__ (self, x):
        """Convert to a standard Python real."""
        try:
            return float(x)
        except Exception, e:
            raise OverflowError, e

class DoubleComplexKindObject (KindObject):
    "Kind object for a floating type corresponding to C doublecomplex."
    def __init__ (self):
        "Create a complex kind object using the given float kind and conversion routine."
        KindObject.__init__(self,
                            name='doublecomplex',
                            typecode='D'
                            )
        self.__floatkind = DoubleKindObject()

    def float_kind(self):
        return self.__floatkind

    def __call__ (self, x, y=None):
        "Convert to a standard Python complex."
        try:
            if y is None:
                return complex(x)
            else:
                return complex(x,y)
        except Exception, e:
            raise OverflowError, e

# this is just here as a starting point for future expansion...
class FloatKindObject (KindObject):
    "Kind object for a floating type corresponding to C float."
    def __init__ (self):
        """Create the kind object corresponding with C float."""
        raise RuntimeError, 'float kind not supported'
        KindObject.__init__ (self,
                         name='float', 
                         typecode='f',
                         MAX=_kinds.FLT_MAX, 
                         MIN=_kinds.FLT_MIN,
                         DIG=_kinds.FLT_DIG,
                         EPSILON=_kinds.FLT_EPSILON,
                         MAX_EXP=_kinds.FLT_MAX_EXP,
                         MIN_EXP=_kinds.FLT_MIN_EXP,
                         MAX_10_EXP=_kinds.FLT_MAX_10_EXP,
                         MIN_10_EXP=_kinds.FLT_MIN_10_EXP,
                         RADIX=_kinds.FLT_RADIX,
                         ROUNDS=_kinds.FLT_ROUNDS)

    def __call__ (self, x):
        """Convert to floating type corresponding to C float."""
        try:
            return float32(x) # does not exist in standard release
        except Exception, e:
            raise OverflowError, e

__intkind = IntKindObject()
__longkind = LongKindObject()
__doublekind = DoubleKindObject()
__doublecomplexkind = DoubleComplexKindObject()

int_kinds = [__intkind, __longkind]
float_kinds = [__doublekind]
complex_kinds = [__doublecomplexkind]

default_int_kind=__intkind
default_long_kind = __longkind
default_float_kind = __doublekind
default_complex_kind = __doublecomplexkind

def int_kind (n):
    """Return a kind object corresponding to an integer kind holding numbers
       with n decimal digits.
       If n is zero, return default_int_kind.
    """
    if n < 0: 
        raise ValueError, 'int_kind argument must be non-negative'
    elif n == 0: 
        return default_int_kind
    nl = 10L**(n)-1
    for ikind in int_kinds[:-1]:
        if nl <= long(ikind.MAX) and \
           -nl >= long(ikind.MIN):
            return ikind
    else:
        return default_long_kind


def float_kind(nd, n):
    """Return a floating kind object with at least nd digits of precision 
       that will hold numbers in the range [-10**n, 10**n], in which
       1.e-n is not zero.

       If nd = n = 0, return kind corresponding to Python default floating type.
    """
    if nd < 0 or n <0:
        raise ValueError, 'float_kind arguments must be non-negative.'
    if n == 0 and nd == 0:
        return default_float_kind
    for akind in float_kinds:
        if nd <= akind.DIG and \
            n <=  akind.MAX_10_EXP and \
            -n >= akind.MIN_10_EXP:
            return akind
    else:
        raise OverflowError, 'No such floating kind available.'

def complex_kind(nd, n):
    """Return a complex kind object with at least nd digits of precision 
       that will hold numbers in the range [-10**n, 10**n], in which
       1.e-n is not zero.
    """
    if nd < 0 or n <0:
        raise ValueError, 'complex_kind arguments must be non-negative.'
    if n == 0 and nd == 0:
        return default_complex_kind
    for ckind in complex_kinds:
        akind = ckind.floatkind
        if nd <= akind.DIG and \
            n <=  akind.MAX_10_EXP and \
            -n >= akind.MIN_10_EXP:
            return ckind  
    else:
        raise OverflowError, 'No such complex kind available.'

