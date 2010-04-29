import PropertiedClasses
def validstring (instance, name, value):
    if not value: raise ValueError, name

class Test (PropertiedClasses.PropertiedClass):
    def __init__ (self, initial_shape):
        self.__s = initial_shape
        self._basic_set('ro', 9)
        self._basic_set('ro2', 9)
        self._basic_set('ro3', 9)
        self.alwayspositive = 1
        self.kenny = 1
        self.vs = 'valid'
    
    def getShape (self, name):
        assert name == 'shape'
        return self.__s

    def setShape (self, name, newshape):
        assert name == 'shape'
        self.__s = newshape

    def validate (self, name, value):
        "This gatekeeper insures alwayspositive is always positive."
        if value < 0:
            raise ValueError, "You cannot set alwayspositive < 0."
        self._basic_set ('alwayspositive', value)

    def kill (self, name):
        """Delete handler for 'kenny'."""
        assert name == 'kenny'
        pass
        
class Test2 (Test):
    def __init__ (self, initial_shape):
        Test.__init__ (self, initial_shape)
        self.__s = initial_shape

    def validate (self, name, value):
        "This gatekeeper insures name is always in (0...9)"
        if value < 0 or value >= 10:
            raise ValueError, "Improper value for %s." % (name,)
        self._basic_set (name, value)

class Test3 (Test2):
    def __init__ (self, initial_shape):
        self._basic_set ('_d', {})
        Test2.__init__(self, initial_shape)
        

    def __setattr__ (self, name, value):
        self._basic_get('_d')[name] = value
        Test2.__setattr__(self, name, value)
        
class Test4 (PropertiedClasses.PropertiedClass):
    def _basic_set (self, name, value):
        "Restrict acceptable attribute names"
        if name in ['a','b','c']:
            PropertiedClasses.PropertiedClass._basic_set (self, name, value)
        else:
            raise AttributeError, name + ' is not settable.'

PropertiedClasses.set_property (Test, 'shape', Test.getShape, Test.setShape)
assert PropertiedClasses.get_property_g(Test, 'shape').im_func is  Test.getShape.im_func
PropertiedClasses.set_property (Test, 'alwayspositive', acts=Test.validate)
PropertiedClasses.set_property (Test, 'ro', nowrite=1)
PropertiedClasses.set_property (Test, 'ro3', acts = Test.validate)
PropertiedClasses.set_property (Test, 'ro2', nowrite=1)
PropertiedClasses.set_property (Test, 'ro3', nodelete=1)
PropertiedClasses.set_property (Test, 'kenny', actd=Test.kill)
PropertiedClasses.set_property (Test, 'vs', acts=validstring)
PropertiedClasses.set_property (Test2, 'alwayspositive', acts=Test2.validate)
assert PropertiedClasses.get_property_s(Test, 'alwayspositive').im_func is \
                                 Test.validate.im_func
assert PropertiedClasses.get_property_s(Test2, 'alwayspositive').im_func is \
                                 Test2.validate.im_func

t = Test((3,2))
t.a = 1
assert t.a == 1
assert t.shape == (3,2)
t.shape = (4,4)
assert t.shape == (4,4)
t.alwayspositive = 5
assert t.alwayspositive == 5
try:
    t.alwayspositive = -1
except ValueError:
    pass
try:
    del t.shape
except AttributeError:
    pass
assert t.ro == 9
try:
    t.ro = 8
    raise SystemExit, 'read-only ro failed to test correctly.'
except AttributeError:
    pass
assert t.ro2 == 9
try:
    t.ro2 = 8
    raise SystemExit, 'read-only ro2 failed to test correctly.'
except AttributeError:
    pass
try:
    del t.ro2
    raise SystemExit, 'delete of ro2 failed to test correctly.'
except AttributeError:
    pass
try:
    t.ro3 = 8
except AttributeError:
    raise SystemExit, 'writeablitity of ro3 failed to test correctly.'
try:
    t.ro3 = -1
    raise SystemExit, 'Validation on r3 failed to test correctly.'
except ValueError:
    pass
try:
    del t.ro3
    raise SystemExit, 'Delete protection on r3 failed to test correctly.'
except AttributeError:
    pass
del t.kenny 
assert hasattr(t, 'kenny')
try:
    t.vs = ''
    raise SystemExit, 'external validator failed.'
except ValueError:
    pass

t2 = Test2(t.shape)
t2s = t2.shape
ts = t.shape
assert t2s == ts
t2.alwayspositive = 5
assert t2.alwayspositive == 5
try:
    t2.alwayspositive = 12
except ValueError:
    pass
assert t2.alwayspositive == 5
import pickle
s = pickle.dumps(t2)

t3 = Test3((4,5))
t3.shape = (4,3)
assert t3.shape == (4,3)
PropertiedClasses.show_properties(Test3)

t4 = Test4()
t4.a = 1
try:
    t4.aaa = 2
    raise RuntimeError, 'Test4 test failed.'
except AttributeError:
    pass
print "Test passed."
