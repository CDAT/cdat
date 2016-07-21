## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"Read part of the old cu slab interface implemented over CDMS"
import numpy
import string, types, sys
from error import CDMSError
from axis import std_axis_attributes
import cdms2 as cdms

class Slab:
    """Slab is the cu api
       This is an abstract class to inherit in AbstractVariable
       About axes:
       weight and bounds attributes always set but may be None
       if bounds are None, getdimattribute returns result of querying the 
       axis.
    """
    std_slab_atts = ['filename',
                'missing_value',
                'comments',
                'grid_name',
                'grid_type',
                'time_statistic',
                'long_name',
                'units']
    def __init__ (self):
        pass

    def getattribute (self, name):
        "Get the attribute name."
        defaultdict = {'filename':'N/A',
                       'comments':'',
                       'grid_name':'N/A',
                       'grid_type':'N/A',
                       'time_statistic':'',
                       'long_name':'',
                       'units':''}
        result = None
        if name in defaultdict.keys() and not hasattr(self,name):
            if name=='filename':
                if (not hasattr(self,'parent')) or self.parent is None:
                    result = ''
                else:
                    result = self.parent.id

            elif name=='grid_name':
                grid = self.getGrid()
                if grid is None:
                    result = defaultdict[name]
                else:
                    result = grid.id
            elif name=='grid_type':
                grid = self.getGrid()
                if grid is None:
                    result = defaultdict[name]
                elif isinstance(grid,cdms.grid.TransientRectGrid):
                    result = grid.getType()
                elif isinstance(grid,cdms.gengrid.AbstractGenericGrid):
                    result = 'GenericGrid'
                elif isinstance(grid,cdms.hgrid.AbstractCurveGrid):
                    result = 'CurvilinearGrid'
            else:
                result = defaultdict[name]
        else:
            try:
                result = getattr(self, name)
            except AttributeError:
                result = None
            
        return result

    def setattribute (self, name, value):
        "Set the attribute name to value."
        setattr(self, name, value)

    def createattribute (self, name, value):
        "Create an attribute and set its name to value."
        setattr(self, name, value)

    def deleteattribute (self, name):
        "Delete the named attribute."
        if hasattr(self, name):
            delattr(self, name)

    def listattributes (self):
        "Return a list of attribute names."
        return self.attributes.keys()

    def listdimattributes(self, dim):
        "List the legal axis field names."
        a = self.getAxis(dim)
        result = []
        for x in std_axis_attributes + a.attributes.keys():
            if not x in result: result.append(x)
        return result

    def getdimattribute (self, dim, field):
        """Get the attribute named field from the dim'th dimension.
         For bounds returns the old cu one-dimensional version.
        """
        d = self.getAxis(dim)
        if field == "name":
            return d.id

        elif field == "values":
            return d[:]

        elif field == "length":
            return len(d)

        elif field == "units":
            # We can't make axes always have units because cdtime chokes
            return getattr(d, 'units', '')

        elif field == "weights":
            g = self.getGrid()
            if g is None:
                return numpy.ones(len(d))
            if d.isLatitude():
                return g.getWeights()[0]
            elif d.isLongitude():
                return g.getWeights()[1]
            else: #should be impossible, actually
                return numpy.ones(len(d))

        elif field == "bounds":
            b = d.getBounds()
            n = b.shape[0]
            result = numpy.zeros(n+1, b.dtype.char)
            result[0:-1] = b[:,0]
            result[-1] = b[-1,1]
            return result
        elif d.attributes.has_key(field):
            return d.attributes[field]
        else:
            raise CDMSError, "No %s attribute on given axis." % field
            
          
    def showdim(self):
        "Show the dimension attributes and values." 
        result = []
        for nd in range(self.rank()):
            result.append('** Dimension ' + str(nd+1) + ' **')
            result = result + self.getAxis(nd).listall(1)
        print string.join(result, '\n')

    def listdimnames(self):
        "Return a list of the names of the dimensions."
        result=[]
        for nd in range(self.rank()):
            result.append(self.getdimattribute(nd, 'name'))
        return result

    def listall (self, all=None):
        "Get list of info about this slab."
        vname = self.id
        result = []
        result.append('*** Description of Slab %s ***' % vname)
        result.append('id: ' + vname)
        result.append('shape: ' + str(self.shape))
        for x in Slab.std_slab_atts:
            result.append(x + ": " + str(self.getattribute(x)))
        for x in self.attributes.keys():
            if x in Slab.std_slab_atts: continue
            if x == 'name': continue
            result.append(x + ": " + str(self.attributes[x]))
        g = self.getGrid()
        if g is None:
            result.append('No grid present.')
        else:
            result = result + g.listall(all)
        for nd in range(self.rank()):
            result.append('** Dimension ' + str(nd+1) + ' **')
            result = result + self.getAxis(nd).listall(all)
        result.append('*** End of description for %s ***' % vname)
        return result

    def info(self, flag=None, device=None):
        "Write info about slab; include dimension values and weights if flag"
        if device is None: device = sys.stdout
        device.write(string.join(self.listall(all=flag), "\n"))
        device.write("\n")

def cdms_bounds2cu_bounds (b):
    "Bounds are  len(v) by 2 in cdms but len(v)+1 in cu"
    cub = numpy.ma.zeros(len(b)+1, numpy.float32)
    b1 = b.astype(numpy.float32)
    if len(b)>1:
        if (b[0,0]<b[0,1]) == (b[0,0]<b[-1,0]):
            cub[0] = b[0,0]
            cub[1:] = b[:,1]
        else:
            cub[0] = b[0,1]
            cub[1:] = b[:,0]
    else:
        cub[:] = b[0]
    return numpy.array( cub )
    
