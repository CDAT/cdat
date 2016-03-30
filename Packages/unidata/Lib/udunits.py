import udunits_wrap
import sys
import string
import unidata
from collections import OrderedDict
version = sys.version.split()[0].split('.')
version = string.join(version[:2], '.')
udunits_name = unidata.__path__[0] + '/udunits.dat'
known_units_units = None
known_units_types = None


def _readUnits():
    f = open(udunits_name)
    units = {}
    type = '???'
    types = {}
    ln = f.readlines()
    f.close()
    for i in range(len(ln)):
        if ln[i][0] == '#':
            if ln[i] == '#\n' and ln[i + 1][0] == '#' and ln[i + 2] == '#\n':
                type = ln[i + 1][2:-1]
                if type[:8] == 'UNITS OF':
                    type = type[8:].strip()
        elif not ln[i][0] in ['\n', ' ']:
            unit = ln[i].split()[0]
            units[unit] = type
            tmp = types.get(type, [])
            tmp.append(unit)
            types[type] = tmp
    units = OrderedDict(sorted(units.iteritems()))
    types = OrderedDict(sorted(types.iteritems()))
    return units, types


def addBaseUnit(name):
    """let you add to udunits2 system a new base unit with a specific name"""
    return udunits_wrap.addBaseUnit(name)


def addDimensionlessUnit(name):
    """let you add to udunits2 system a new dimensionless unit with a specific name"""
    return udunits_wrap.addDimensionlessUnit(name)


def addScaledUnit(name, scale, original):
    """let you add to udunits2 system a new scaled unit with a specific name
    from an original unit"""
    return udunits_wrap.addScaledUnit(name, scale, original)


def addOffsettedUnit(name, offset, original):
    """let you add to udunits2 system a new offsetted unit with a specific name
    from an original unit"""
    return udunits_wrap.addOffsettedUnit(name, offset, original)


def addMultipliedUnits(name, unit1, unit2):
    """let you add to udunits2 system a new unit with a specific name
    which is the multiplication of two other units specified by name"""
    return udunits_wrap.addMultipliedUnits(name, unit1, unit2)


def addDividedUnits(name, unit1, unit2):
    """let you add to udunits2 system a new unit with a specific name
    which is the division of two other units specified by name"""
    return udunits_wrap.addDividedUnits(name, unit1, unit2)


def addInvertedUnit(name, original):
    """let you add to udunits2 system a new inverted unit with a specific name
    from an original unit"""
    return udunits_wrap.addInvertedUnit(name, original)


class udunits(object):
    """UNIDATA/UDUNITS Python Object

    initialization: a=unidata(value,units)

    availabe commands:
    all of ufuncs are avalailable:
    +,-,/,*, appropriate conversion to left side units is done on the fly
    example:
    a=unidata.udunits(5,'m')
    b=unidata.udunits(6,'in')
    c=a+b # udunits(5.1524,"m")
    # but
    c=b+a # udunits(202.850393701,"in")

    # wrong units conversion will generate errors:
    c=unidata.udunits(7,'K')
    Traceback (most recent call last):
    File "<stdin>", line 1, in ?
    File "/lgm/cdat/latest/lib/python2.3/site-packages/unidata/udunits.py", line 111, in __add__
    s,i=udunits_wrap.convert(other.units,self.units)
    TypeError: Error Converting.

    #a*c # works

    Note: a/b # returns a number (unitless)
    a+c #


    OTHER FUNCTION

    UNIT CONVERSION
    a.units='feet'
    print a # 16.4041994751 feet

    c=a.to('km') # udunits(0.005,"km")

    c=unidata.udunits(7,'K')
    factor, offset = c.how('degF') # (1.7999999999999998, -459.67000000000002)

    WHICH UNITS ?
    By available (known) units
    lst = c.available_units() #  returns list of all known units

    By units:
    dict = c.known_units() # returns a dictionary of units (keys) associated with type (values)
    dict['k'] # returns : 'THERMODYNAMIC TEMPERATURE'

    By type of units
    # returns a dictionary of units type (keys) associated with a list of units for each type
    dict = c.known_units(bytype=1)
    e.g
    dict['THERMODYNAMIC TEMPERATURE'] # returns['degree_Kelvin', 'degree_Celsius', ...]

    """
    __slots__ = ['units', '_units', 'value', '_value']

    def __init__(self, value, units):
        if isinstance(value, str):
            if isinstance(units, (int, long, float)):
                tmp = units
                units = value
                value = tmp
            else:
                if units is None:
                    units = value
                    value = None
                else:
                    units = value

        if isinstance(units, (int, long, float)):
            raise 'Error, must provide at least one args as string representing units'

        if unidata.udunits_init == 0:
            udunits_wrap.init()  # udunits_name)
            unidata.udunits_init = 1
        self._units = units
        self._value = float(value)

    def to(self, units):
        if isinstance(units, udunits):
            u = units.units
        else:
            u = units
        s, i = udunits_wrap.convert(self.units, u)
        return udunits(self.value * s + i, u)

    def how(self, units):
        if isinstance(units, udunits):
            u = units.units
        else:
            u = units
        s, i = udunits_wrap.convert(self.units, u)
        return s, i

    def available_units(self):
        out = self.known_units().keys()
        return out

    def known_units(self, bytype=0):
        global known_units_units, known_units_types
        if known_units_units is None:
            known_units_units, known_units_types = _readUnits()
        if bytype == 1:
            return known_units_types
        else:
            return known_units_units

    def _getunits(self):
        return self._units

    def _setunits(self, units):
        if self.value is None:
            self._units = units
        elif units != self.units:
            s, i = udunits_wrap.convert(self.units, units)
            self.value = self.value * s + i
            self._units = units
        return
    units = property(_getunits, _setunits)

    def __str__(self):
        return str(self.value) + ' ' + str(self.units)

    def __repr__(self):
        return "udunits(" + str(self.value) + ',"' + str(self.units) + '")'

    def _getvalue(self):
        return self._value

    def _setvalue(self, value):
        self._value = value
        return
    value = property(_getvalue, _setvalue)

    def __add__(self, other):
        if not isinstance(other, (udunits, int, long, float)):
            raise "Error must add a number or a udunit object"
        out = udunits(self.units, self.value)
        if isinstance(other, udunits):
            s, i = udunits_wrap.convert(other.units, self.units)
            out.value = self.value + other.value * s + i
        else:
            out.value += other
        return out

    def __radd__(self, other):
        if not isinstance(other, (udunits, int, long, float)):
            raise "Error must add a number or a udunit object"
        out = udunits(self.units, self.value)
        if isinstance(other, udunits):
            s, i = udunits_wrap.convert(other.units, self.units)
            out.value = self.value + other.value * s + i
        else:
            out.value += other
        return out

    def __sub__(self, other):
        if not isinstance(other, (udunits, int, long, float)):
            raise "Error must sub a number or a udunit object"
        out = udunits(self.units, self.value)
        if isinstance(other, udunits):
            s, i = udunits_wrap.convert(other.units, self.units)
            out.value = self.value - (other.value * s + i)
        else:
            out.value -= other
        return out

    def __rsub__(self, other):
        if not isinstance(other, (udunits, int, long, float)):
            raise "Error must sub a number or a udunit object"
        out = udunits(self.units, self.value)
        if isinstance(other, udunits):
            s, i = udunits_wrap.convert(other.units, self.units)
            out.value = (other.value * s + i) - self.value
        else:
            out.value = other - out.value
        return out

    def __mul__(self, other):
        if not isinstance(other, (udunits, int, long, float)):
            raise "Error must multiply a number or a udunit object"
        out = udunits(self.units + '*' + self.units, self.value)
        if isinstance(other, udunits):
            try:
                s, i = udunits_wrap.convert(other.units, self.units)
                out.value = self.value * (other.value * s + i)
            except:  # Ok uncompatible units, just do the produce
                out = udunits(
                    self.units + '*' + other.units,
                    self.value * other.value)
        else:
            out = udunits(other * self.value, self.units)
        return out

    def __rmul__(self, other):
        if not isinstance(other, (udunits, int, long, float)):
            raise "Error must multiply a number or a udunit object"
        out = udunits(self.units + '*' + self.units, self.value)
        if isinstance(other, udunits):
            try:
                s, i = udunits_wrap.convert(other.units, self.units)
                out.value = self.value * (other.value * s + i)
            except:  # Ok uncompatible units, just do the produce
                out = udunits(
                    self.units + '*' + other.units,
                    self.value * other.value)
        else:
            out = udunits(other * self.value, self.units)
        return out

    def __div__(self, other):
        if not isinstance(other, (udunits, int, long, float)):
            raise "Error must divide by a number or a udunit object"
        out = udunits(self.units, self.value)
        if isinstance(other, udunits):
            try:
                s, i = udunits_wrap.convert(other.units, self.units)
                out = self.value / (other.value * s + i)
            except:  # Ok uncompatible units, just do the produce
                out = udunits(
                    self.value / other.value,
                    self.units + '/' + other.units)
        else:
            out.value /= other
        return out

    def __rdiv__(self, other):
        if not isinstance(other, (udunits, int, long, float)):
            raise "Error must divide by a number or a udunit object"
        out = udunits(self.units, self.value)
        if isinstance(other, udunits):
            try:
                s, i = udunits_wrap.convert(other.units, self.units)
                out = (other.value * s + i) / self.value
            except:  # Ok uncompatible units, just do the produce
                out = udunits(
                    other.value / self.value,
                    other.units + '/' + self.units)
        else:
            out.value = other / self.value
            out._units = '1/(' + self.units + ')'
        return out

    def __pow__(self, other):
        if not isinstance(other, (int, long, float)):
            raise "Error must power to a number"
        out = udunits(self.value ** other, self.units + '**' + str(other))
        return out
