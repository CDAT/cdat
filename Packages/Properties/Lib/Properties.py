"""Classes that inherit from PropertiedClass can use attribute names 
   to trigger actions rather than to simply get or 
   set the attribute. (Similar to a property in Visual Basic, or a 
   parameterless method in Eiffel).

   One simple use is for attribute validation. This facility can also
   be used to make an attribute read-only or undeleteable.

   A property attribute is an attribute 'a' for which methods have been 
   supplied to handle any or all of these situations:
        x.a        ('get' the attribute)
        x.a = v    ('set' the attribute)
        del x.a.   ('delete' the attribute)
   
   A "handler" for each of these is a method in the class with the following
   signatures:
       aget (self, name)  
       aset (self, name, value) 
       adel (self, name) 

   A function that is not a method of the class can also be used if it has 
   the correct signature.

   An attribute that is not a property is a 'basic' attribute and is handled
   by direct manipulation of the instance's __dict__.
 
   Routines _basic_get, _basic_set, and _basic_del are available to use
   when you wish to avoid the property handler. These
   three methods use the instance dictionary directly.

   Usage:
       Have your class inherit from PropertiedClass.

       Use methods _basic_set, _basic_get, _basic_del to evade your own 
       handlers when necessary.

       After class is defined, make calls to set_property. This is
       available as a function or method. 
   
       A child can override a parent's handler for a given name.

      If a class inherits from PropertiedClass and wishes to redefine 
      __getattr__, __setattr__, or __delattr__, great care is needed.
      The property behavior will be lost unless the PropertiedClass versions
      are invoked in the new version.
"""
import sys, types

def initialize_property_class (C):
    """
    Turn on the property handler in class C.
    """
    if not issubclass(C, PropertiedClass):
        raise TypeError, \
              'Argument must be a class inheriting from PropertiedClass'
    # Note that we have to make sure C itself has properties dicts, not one
    # of its parents.
    if C.__dict__.has_key('_properties_g'):
        return
# The _properties_x map names to handlers.
# They are filled in with those from the parent classes.
    C._properties_g = {}
    C._properties_s = {}
    C._properties_d = {}
    for P in C.__bases__:
        if issubclass(P, PropertiedClass):
            if P is PropertiedClass: continue  #no need for it, no handlers.
            initialize_property_class(P) 
            for name in P._properties_g.keys():
                if not C._properties_g.has_key(name):
                    C._properties_g[name] = P._properties_g[name]
            for name in P._properties_s.keys():
                if not C._properties_s.has_key(name):
                    C._properties_s[name] = P._properties_s[name]
            for name in P._properties_d.keys():
                if not C._properties_d.has_key(name):
                    C._properties_d[name] = P._properties_d[name]


def set_property (C, name, actg=None, acts=None, actd=None,
                           nowrite=None, nodelete=None):
    """Set attribute handlers for  name to methods actg, acts, actd
       None means no change for that action.
       nowrite = 1 prevents setting this attribute.
           nowrite defaults to 0. 
       nodelete = 1 prevents deleting this attribute.
           nodelete defaults to 1 unless actd given.
       if nowrite and nodelete is None: nodelete = 1
    """
    initialize_property_class (C)
    for h in (actg, acts, actd):
        if h is None: continue
    if nowrite and nodelete is None: 
        nodelete = 1
    if nowrite and acts is not None:
        raise ValueError, 'nowrite not consistent with acts specification.'
    if nodelete and actd is not None:
        raise ValueError, 'nodelete not consistent with actd specification.'
    if nowrite: 
        acts = C._write_disallowed
    if nodelete: 
        actd = C._delete_disallowed
    if actg is not None: 
        C._properties_g[name] = actg
    if acts is not None: 
        C._properties_s[name] = acts
    if actd is not None: 
        C._properties_d[name] = actd

def get_property_g (C, name):
    """Return get handler for name in C assuming it is not internal"""
    initialize_property_class (C)
    try:
        d = C._properties_g
        f = d[name]
        return f
    except AttributeError:  #no properties 
        return None
    except KeyError:
        return None
        
def get_property_s (C, name):
    """Return set handler for name in C assuming it is not internal"""
    initialize_property_class (C)
    try:
        d = C._properties_s
        return d[name]
    except AttributeError:  #no properties 
        return None
    except KeyError:
        return None
        
def get_property_d (C, name):
    """Return del handler for name in C assuming it is not internal"""
    initialize_property_class (C)
    try:
        d = C._properties_d
        return d[name]
    except AttributeError:  #no properties 
        return None
    except KeyError:
        return None
        
def get_properties (C, name=None):
    """Return dictionary mapping names to the handlers for class C; 
       if name given, just for that name.
    """
    if not issubclass(C, PropertiedClass): 
        return []
    initialize_property_class (C)
    names = []
    if name is None:
        for d in (C._properties_g, 
                  C._properties_s, 
                  C._properties_d
                 ):
            for n in d.keys():
                if n not in names: 
                    names.append(n)
    else:
        names.append(name)
    result = {}
    for n in names:
         result[n] = (get_property_g(C, n), 
                      get_property_s(C, n),
                      get_property_d(C, n)
                     )
    return result

def show_properties(C, name=None):
    """Print the result of get_properties(C, name) in a table."""
    if not isinstance(C, types.ClassType):
        try:
            C = C.__class__
        except AttributeError:
            print 'Object given is not an instance or class and hence has no property handlers.'
            return
    r = get_properties(C, name)
    names = r.keys()
    names.sort()
    for n in names:
        print n
        h = r[n]
        if h[0] is not None:
            print "    get", h[0]
        if h[1] is not None:
            print "    set", h[1]
        if h[2] is not None:
            print "    del", h[2]

class PropertiedClass:
    def __getattr__ (self, name):
        g = self.get_property_g (name)
        if g is None:
            return self._basic_get(name)
        else:
            return g(self, name)

    def __setattr__ (self, name, value):
        s = self.get_property_s (name)
        if s is None:
            self._basic_set(name, value)
        else:
            s(self, name, value)
   
    def __delattr__ (self, name): 
        d = self.get_property_d (name)
        if d is None:
            return self._basic_del(name)
        else:
            d(self, name)

    def _basic_get (self, name):
        """Fundamental attribute getter. 
           Do not invoke directly on private name.
           Evades property mechansim of this class.
        """
        try:
            return self.__dict__[name]
        except KeyError:
            raise AttributeError, "%s instance has no attribute %s" %\
                                      (self.__class__.__name__, name)

    def _basic_set (self, name, value):
        """Fundamental attribute setter. 
           Do not invoke directly on private name.
           Evades property mechanism of this class.
        """
        self.__dict__[name] = value

    def _basic_del (self, name):
        """Fundamental attribute deleter. 
           Do not invoke directly on private name.
           Evades property mechanism of this class.
        """
        try:
            del self.__dict__[name]
        except KeyError:
            raise AttributeError, "%s instance has no attribute %s" %\
                                      (self.__class__.__name__, name)

    def is_internal_attribute (self, name):
        """If self.is_internal_attribute(name), name does not use the 
           property handler. Default version: any name that starts with _."""
        return name[0] == '_'

    def _write_disallowed(self, name, value=None):
        "Use this as an acts to prevent write of an attribute."
        raise AttributeError, \
              'Write forbidden on %s in class %s.' % \
              (name, self.__class__.__name__,)

    def _delete_disallowed(self, name, value=None):
        "Use this as an actd to prevent delete of an attribute."
        raise AttributeError, \
              'Write forbidden on %s in class %s.' % \
              (name, self.__class__.__name__,)

    def set_property (self, name, actg=None, acts=None, actd=None,
                      nowrite=None, nodelete=None):
        """Set attribute handlers for  name to methods actg, acts, actd
           None means no change for that action.
           nowrite = 1 prevents setting this attribute.
               nowrite defaults to 0. 
           nodelete = 1 prevents deleting this attribute.
               nodelete defaults to 1 unless actd given.
           if nowrite and nodelete is None: nodelete = 1
        """
        set_property (self.__class__, name, actg, acts, actd, nowrite, 
                      nodelete)

    def get_property_g (self, name):
        """Return the 'get' property handler for name that self uses.
           Returns None if no handler.
        """
        if self.is_internal_attribute (name):
            return None
        else:
            return get_property_g(self.__class__, name)

    def get_property_s (self, name):
        """Return the 'set' property handler for name that self uses.
           Returns None if no handler.
        """
        if self.is_internal_attribute (name):
            return None
        else:
            return get_property_s(self.__class__, name)

    def get_property_d (self, name):
        """Return the 'del' property handler for name that self uses.
           Returns None if no handler.
        """
        if self.is_internal_attribute (name):
            return None
        else:
            return get_property_d(self.__class__, name)

PropertiedClass.__doc__ = __doc__
