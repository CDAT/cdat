"InternalAttributes (implmentation class for CDMS)"
import types
import PropertiedClasses
_PCLASS = PropertiedClasses.PropertiedClass
class AttributeDict:
    """An attribute dictionary."""
    def __init__ (self, owner):
        self._owner = owner
    
    def __getitem__ (self, name):
        if self.has_key(name):
            return self._owner.__dict__[name]
        else:
            raise KeyError, "%s instance has no external attribute %s" % \
                   (self._owner.__class__.__name__, name)

    def __setitem__ (self, name, value):
        if self._owner.is_internal_attribute(name):
            raise RuntimeError, 'Cannot set internal name in external attribute dictionary.'
        self._owner.__dict__[name] = value

    def clear (self):
        self._owner.__dict__.clear()

    def get (self, name, default=None):
        if self.has_key(name):
            return self._owner.__dict__[name]
        else:
            return default

    def has_key(self, name):
        d = self._owner.__dict__
        if d.has_key(name) and not self._owner.is_internal_attribute(name):
            return 1
        else:
            return 0

    def items (self):
        result = []
        for name, value in self._owner.__dict__.items():
            if self._owner.is_internal_attribute(name): continue
            result.append((name, value))
        return result
    
    def keys (self):
        result = []
        for name in self._owner.__dict__.keys():
            if self._owner.is_internal_attribute(name): continue
            result.append(name)
        return result

    def update(self, d):
        for name, value in d.items():
            if self._owner.is_internal_attribute(name):
                raise RuntimeError, "Cannot update attribute dict with internal name"
        self._owner.__dict__[name] = value

    def values (self):
        result = []
        for name, value in self._owner.__dict__.items():
            if self._owner.is_internal_attribute(name): continue
            result.append(value)
        return result

    def __repr__(self):
        return 'AttributeDict (' + \
        repr(self._owner.__dict__) + \
        ')'

    def __str__(self):
        return str(self._owner.__dict__)
    
class InternalAttributesClass (_PCLASS):
    def _getattributes (self, name):
        """Return a dictionary-like object of the non-internal attributes."""
        return AttributeDict(self)

    def is_internal_attribute (self, name):
        """is_internal_attribute(name) is true if name is internal."""
        if name[0] == '_' or name in self.__class__._internal:
            return 1
        return 0

    def replace_external_attributes(self, newAttributes):
        """replace_external_attributes(newAttributes)
           Replace the external attributes with dictionary newAttributes.
        """
        if not isinstance(newAttributes, types.DictType) and \
           not isinstance(newAttributes, AttributeDict):
            raise ValueError, "Argument must be a dictionary"
        for n in self.__dict__.keys():
            if not self.is_internal_attribute(n):
                del self.__dict__[n]
        for n, v in newAttributes.items():
            self.__dict__[n] = v

def initialize_internal_attributes (C):
    "Prepare a class for life as a child of InternalAttributesClass."
    if C.__dict__.has_key('_internal'): return
    if not issubclass(C, InternalAttributesClass):
        raise ValueError, 'Must be subclass of InternalAttributesClass'
    PropertiedClasses.initialize_property_class (C)
    C._internal = []
    for CP in C.__bases__:
        if issubclass(CP, InternalAttributesClass):
            initialize_internal_attributes(CP)
            for name in CP._internal:
                C._internal.append(name)
    
def add_internal_attribute (C, *aname):
    """add_internal_attribute (C, name, ...)
       Make attributes name, ... internal in class C.
    """
    initialize_internal_attributes(C)
    for name in aname:
        if not name in C._internal:
            C._internal.append(name)

PropertiedClasses.set_property(InternalAttributesClass, 'attributes', 
                               InternalAttributesClass._getattributes, 
                               nowrite=1, nodelete=1)

if __name__ == '__main__':
    class Test(InternalAttributesClass):
        def __init__ (self):
            self.node = None
            self.parent = None
            self.__dict__['ro'] = 1
            self.__hide = 3
            self._p = 4
            self.value = 1

    PropertiedClasses.set_property(Test, 'ro', nowrite=1, nodelete=1)
    add_internal_attribute(Test, 'node', 'parent', 'ro')

    t1 = Test()
    assert t1.value == 1
    assert not t1.attributes.has_key('__hide')
    assert not t1.attributes.has_key('_p')
    assert t1._p == 4
    t1.value = 2
    assert t1.value == 2
    assert 'value' in t1.attributes.keys()
    t1.b = t1.value + 1
    assert t1.b == 3
    assert t1.b == t1.attributes['b']
    t1.node = 'me'
    t1.parent = 'dad'
    assert t1.node == 'me'
    assert 'node' not in t1.attributes.keys()
    assert t1.ro == 1
    try:
        t1.ro == 2
    except AttributeError:
        pass
    assert t1.ro == 1
    print "Test passed."
