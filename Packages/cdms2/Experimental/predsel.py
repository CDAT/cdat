import cdms2,numpy

class PredicateComponent (cdms.selectors.SelectorComponent):
    "A component that confines an axis and selects from it with a predicate."
    def __init__ (self, id, predicate):
        self.id = id
        self.predicate = predicate

    def get_limits (self, axis):
        """Return the slice containing those elements of axis for which
         the predicate is true.
        """
        low = 0
        while low < len(axis):
            if self.predicate(axis, low):
                break
            else:
                low = low + 1
        high = len(axis)
        while high > low:
            if self.predicate(axis, high-1):
                break
            else:
                high = high -1
        return slice(low, high)
            
    def specify (self, slab, axes, specifications, confined_by, aux):
        for i in range(len(axes)):
            if cdms2.axisMatches(axes[i], self.id):
               if confined_by[i] is None:
                   specifications[i] = self.get_limits(axes[i])
                   confined_by[i] = self
                   aux[id(self)] = i
                   return 0
               else:
                   return 1
        return 0

    def post (self, fetched, slab, axes, specifications, confined_by, aux, axismap):
        i = aux[id(self)]
        index = axismap[i]
        fetchedaxes = fetched.getAxisList()
        axis = fetchedaxes[index]
        m = numpy.zeros(fetched.shape,numpy.int32)
        if index <>0:
            p = numpy.arange(numpy.ma.rank(fetched))
            p[0] = index
            p[index] = 0
            m = numpy.transpose(m, p)
        for i in range(len(axis)):
           x = self.predicate(axis, i)
           if not x:
              m[i,...] = 1
        if index <> 0:
           m = numpy.transpose(m, p)
        m = numpy.ma.mask_or(m, numpy.ma.getmask(fetched))
        d = fetched.filled()
        return cdms2.createVariable(d, mask=m, axes=fetchedaxes,
                      attributes = fetched.attributes)
                

if __name__ == "__main__":
    f=cdms2.open('clt.nc')
    def testf (axis, i):
        x = axis[i]
        if x> 10. and x < 20.: 
            result = 0
        else:
            result = (x >= 3. and x <=100.)
        return result
    def test2 (axis, i):
        if axis[i] <= 0.0: 
            return 0
        return 1
    s = cdms2.selectors.Selector(PredicateComponent('time', testf))
    t = f('clt', s, slice(0,1),slice(0,1), squeeze=1)
    northernhemisphere = \
       cdms2.selectors.Selector(PredicateComponent('latitude', test2))
    w = f('clt', northernhemisphere)
    print w.getLatitude()[:]
