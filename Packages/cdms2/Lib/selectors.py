
"""Classes to support easy selection of climate data"""
import string, types, cdtime
from axis import axisMatches
from error import CDMSError
from grid import AbstractRectGrid, defaultRegion, setRegionSpecs, LongitudeType, LatitudeType, TimeType, VerticalType

_debug = 0
class SelectorError (CDMSError):
    "The exception type for errors in the selector packages"
    def __init__ (self, args):
        self.args = args

class Selector:
    """Selector class"""
    def __init__ (self, *args, **kwargs):
        """Positional args are SelectorComponents or Selectors
         Keyword args and their value are passed to kwselect to create
         selectors. All the selector components are put into the 
         components list of this Selector, along with all the components
         of any Selector arguments.
        """
        self.__components = []
        self.refine(*args, **kwargs)
        for a in args:
            if isinstance(a,SelectorComponent):
                try:
                    self.__str__=a.__str__
                except:
                    pass


    def components (self):
        "List of selector components, each an instance of SelectorComponent."
        return self.__components[:]
    
    def refine (self, *args, **kwargs):        
        """Add components to this selector using the same syntax as the 
         constructor. Ignores non-keyword arguments that are not 
         SelectorComponents or Selectors.
        """
        for a in args:
            if a is all:
                continue
            if isinstance(a, SelectorComponent):
                self.__components.append(a)
            elif isinstance(a, Selector):
                for x in a.components():
                    self.refine(x)  
            else:
                self.refine(positionalComponent(a))

        for k, v in kwargs.items():
               self.refine(kwselect(k, v))
 
    def __repr__ (self):
        s = 'Selector('
        sep = ''
        for c in self.__components:
            s = s + sep + repr(c)
            sep = ', '
        return s + ')'
    
    def __and__ (self, other):
        """Implements the & operator, which returns 
           self.clone() refined by other
        """
        if not isinstance(other, Selector):
            raise SelectorError, 'Cannot combine Selector with non-selector'
        s = self.clone()
        s.refine(other)
        return s

    def clone(self):
        "Makes a copy of this Selector."
        return Selector(*self.__components)

    def __call__ (self, *args, **kwargs):
        """Return a new selector consisting of this one refined by the given arguments.
           Arguments are as per the constructor or method refine.
        """
        s = self.clone()
        s.refine(*args, **kwargs)
        return s

    def select(self, variable, *args, **kwargs):
        """Extract the selection from a variable.

           May specify additional refinement via extra arguments and
           keyword specifiers as in the constructor or method 'refine'.

           Options modify the result of the selection. The options and
           their default values are:
               -- raw = 0: if 1, return an numpy.ma only
               -- squeeze = 0:  If 1, eliminate any dimensions of length 1 
                                from the result.
               -- order = None: If given, is a string such as 
                                variable.getOrder()
                                returns. Result is permuted into this order.
               -- grid = None:  If given, is a grid object; result is 
                                regridded onto this grid.
           Each of the components contributes arguments suitable for the
           subRegion call in class cdms.AbstractVariable. If a component 
           is to modify the same axis as a previous component, its application
           is postponed. subRegion is called and the result is then fed
           to each of the components' "post" method. This returns a 
           possibly modified result, which becomes the input to the next
           component's post method. 

           This procedure is repeated until no more components are postponed.
           Then the options are applied to the result in the order
           listed above, and the result is returned. 

           Execption SelectorError is thrown if the selection is
           impossible.

           The result is a TransientVariable and id(variable) <> id(result)
           even if there are no components.
        """  
        d = kwargs.copy()
        raw = d.setdefault('raw', 0)
        squeeze = d.setdefault('squeeze', 0)
        grid = d.setdefault('grid', None)
        order = d.setdefault('order', None)
        del d['squeeze'], d['grid'], d['order'], d['raw']
        # make the selector
        s = self(*args, **d)
        return s.unmodified_select(variable, 
                                   squeeze=squeeze, 
                                   order=order, 
                                   grid=grid, 
                                   raw=raw)

    def unmodified_select(self, variable, raw=0, squeeze=0, order=None, grid=None):
        "Select using this selector without further modification"
        result = variable
        components = self.components()

        # If the grid is non-rectilinear, first select on coordinateComponents
        # that confine the grid.
        vargrid = variable.getGrid()
        if (vargrid is not None) and (not isinstance(vargrid, AbstractRectGrid)):
            newcomponents = []
            specs = defaultRegion()
            for c in components:
                if c.specifyGrid(variable, vargrid, specs): # specs is modified
                    newcomponents.append(c)
            components = newcomponents
            if specs != defaultRegion():
                vgindices = result.getGridIndices()
                mask, indexspecs = vargrid.intersect(specs)
                result = result(**indexspecs)
                result = result.setMaskFromGridMask(mask, vgindices) # Propagate the grid mask to result

        # Now select on non-coordinate components.
        while(components):
            axes = result.getAxisList()
            if _debug: print "Axes:", axes
            specifications = [':']*len(axes)
            confined_by = [None]*len(axes)
            aux = {} # for extra state 
            overflow = []
            if _debug: print "Component list:", components
            for c in components:
                if c.specify(result, axes, specifications, confined_by, aux):
                    if _debug: print 'Defer ' + repr(c)
                    overflow.append(c)
                elif _debug:
                    print "After applying", c, ":"
                    print  "specifications=", specifications
                    print "Confined_by", confined_by
                    print "aux", aux
                    print "-----------------"
            if _debug: 
                print 'About to call subRegion:', specifications
            fetched = result.subRegion(*specifications)
            axismap = range(len(axes))
            for c in components:
                if c in overflow: continue
                fetched = c.post(fetched, result, axes, specifications, 
                                 confined_by, aux, axismap)
            if not len(overflow) < len(components):
                raise SelectorError, \
                  'Internal selector error, infinite loop detected.'
            components = overflow
            result = fetched

        if squeeze != 0 or \
           order is not None or \
           grid is not None or \
           raw !=0 or \
           result is variable: 
     # result is variable when there are no components, for example.
            return result.subRegion(squeeze=squeeze, order=order, grid=grid,
                                    raw=raw)
        else:
            return result
             
class SelectorComponent:
    """Base class representing selection for a given set of axes.
    """
    def specify (self, slab, axes, specifications, confined_by, aux):
        """Refine the specification suitable for slab.subRegion 
           Set confined_by to yourself for each axis you confine.
           If you would normally confine an axis to ':', don't, 
           unless you *require* that axis not be confined by other
           components.
           
           Returning:
              Return 1 if you wish to skip your turn. You'll be called 
              later with the results of the other selectors.

              Raise a SelectorError exception if you can't do your job.

              Otherwise, return 0, even if slab had no axes you wish
              to confine.

              Store any info you want in dictionary aux[id(self)]
        """
        return 0
    
    def specifyGrid(self, var, grid, specs):
        """Refine the specification suitable for grid.intersect().
        
        'var' is a variable.
        'grid' is the grid associated with the variable.
        'specs' is the result set of specifications, of the form defined in the grid module.

        Return:
          0 if self confines the grid.
          1 if self is not associated with coordinate regions, or does not confine the grid.

        Note: This function should return 0 only if self is a component that confines
          nonrectilinear grids. See class coordinateComponent.
        """
        return 1

    def post (self, fetched, slab, axes, specifications, confined_by, aux, axismap):
        """Post-process fetched if desired, return new value.
           Arguments slab, axes, specifications, confined_by, and aux are 
           pre-subRegion call. 
           
           axismap gives the indices of fetched's axes in axes and should 
           be modified as required by this method.  Set axismap[i] to None to
           indicate that you have eliminated an axis.
        """
        return fetched
    
class axisComponent (SelectorComponent):
    "A SelectorComponent that confines exactly one axis or coordinate dimension (e.g. latitude)."
    def __init__ (self, id, spec):
        self.id = id
        self.spec = spec

    def specify (self, slab, axes, specifications, confined_by, aux):
        "Do specification for axis self.id; skip if axis not present."
        for i in range(len(axes)):
            if axisMatches(axes[i], self.id):
               if confined_by[i] is None:
                   specifications[i] = self.spec
                   confined_by[i] = self
                   return 0
               else:
                   return 1
        return 0

    def __repr__ (self):
        s = repr(self.__class__)+'("'+self.id+'", '+repr(self.spec) + ')'
        return s
        
class coordinateComponent(axisComponent):
    "A SelectorComponent that confines exactly one coordinate dimension (e.g., latitude)"

    def __init__(self, id, spec):
        axisComponent.__init__(self, id, spec)

    def specifyGrid(self, var, grid, specs):

        "Determine if this component confines the grid, and if so set the specs and return 1"
        if grid.hasCoordType(self.id):
            setRegionSpecs(grid, self.spec, self.id, specs)
            return 0
        else:
            return 1

class requiredComponent (SelectorComponent):
    """Checks to see that a specific id axis must be present."""
    def __init__ (self, ids):
        """Checks to see that a specific axis or axes must be present.
           Initialize with a sequence of ids.
        """
        self.ids = ids
        
    def specify (self, slab, axes, specifications, confined_by, aux):
        """Doesn't confine but checks for existance."""
        for id in self.ids:
            for i in range(len(axes)):
                if axisMatches(axes[i], id):
                    break
            else:
                raise SelectorError, \
                      'Required axis %s not present in this variable.' % (id,)
        return 0

class indexComponent (axisComponent):
    """An axisComponent that confines exactly one axis by 
       specifying indices. 
    """
    def __init__ (self, id, start=None, stop=None, stride=None):
        self.id = id
        self.spec = slice(start,stop, stride)

class indexedComponent (SelectorComponent):
    """A SelectorComponent that confines exactly one axis  
       whose index is given. 
    """
    def __init__ (self, index, value):
        self.index = index
        self.spec = value

    def specify (self, slab, axes, specifications, confined_by, aux):
        "Do the specification for axis whose index is self.index."
        i = self.index
        if confined_by[i] is None:
            specifications[i] = self.spec
            confined_by[i] = self
            return 0
        else:
            return 1

class positionalComponent (SelectorComponent):
    """A SelectorComponent that confines the next axis available.
    """
    def __init__ (self, v):
        self.v = v

    def specify (self, slab, axes, specifications, confined_by, aux):
        "Find the next unconfined axis and confine it."
        n = 0
        for i in range(len(axes)):
            if confined_by[i] is None:
                specifications[i] = self.v
                confined_by[i] = self
                aux[id(self)] = i
                return 0
        else:
            raise SelectorError, \
            'positional component cannot be applied, insufficent rank:' +\
             repr(self)

    def __repr__ (self):
        s = repr(self.__class__) + '(' + repr(self.v) + ')'
        return s
    
def longitude (*value):
    "Creates default selector corresponding to keyword longitude = value"
    if not value:
        return all
    if len(value) == 1:
        value = value[0]
    if value == ':': return all
    return Selector(coordinateComponent(LongitudeType, value))
    
def latitude (*value):
    "Creates default selector corresponding to keyword latitude = value"
    if not value:
        return all
    if len(value) == 1:
        value = value[0]
    if value == ':': return all
    return Selector(coordinateComponent(LatitudeType, value))
    
def time (*value):
    """Creates a default selector corresponding to keyword time=value
    """
    if not value:
        return all
    if len(value) == 1:
        value = value[0]
    if value == ':': return all
    return Selector(coordinateComponent(TimeType, value))

def level (*value):
    "Creates default selector corresponding to keyword level = value"
    if not value:
        return all
    if len(value) == 1:
        value = value[0]
    if value == ':': return all
    return Selector(coordinateComponent(VerticalType, value))

def required(values):
    """Creates a selector that requires a certain axis to be present."""
    if values is None:
        return all
    if isinstance(values, types.StringType):
        values = (values,)
    return Selector(requiredComponent(values))

def kwselect (k, value):
    """Turn a keyword/value pair into a SelectorComponent
       The words latitude, longitude, time, and level are
       used to pass value to the routine of the same name.
       Otherise, axis is called using k as the id.
    """ 
    kx = k[0:3].lower()
    if kx == 'lat':
        return latitude(value)
    elif kx == 'lon':
        return longitude(value)
    elif k == 'time':
        return time(value)
    elif kx == 'lev':
        return level(value)
    elif kx == 'req':
        return required(value)
    else:
        return Selector(requiredComponent((k,)), axisComponent(k, value))
    
all = Selector()

def timeslice (start=None,stop=None,stride=None):
    return Selector(indexComponent('time', start, stop, stride))
def latitudeslice (start=None,stop=None,stride=None):
    return Selector(indexComponent('latitude', start, stop, stride))
def longitudeslice (start=None,stop=None,stride=None):
    return Selector(indexComponent('longitude', start, stop, stride))
def levelslice (start=None,stop=None,stride=None):
    return Selector(indexComponent('level', start, stop, stride))
def setslice (id, start=None,stop=None,stride=None):
    return Selector(indexComponent(id, start, stop, stride))

