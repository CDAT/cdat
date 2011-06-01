## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"Emulation of old cu package"
import string, types, sys
from error import CDMSError
from dataset import openDataset, createDataset
from tvariable import createVariable
import numpy
import AutoAPI

class cuDataset(AutoAPI.AutoAPI):
    "A mixin class to support the old cu interface"
    def __init__ (self):
        if not hasattr(self,"autoApiInfo"):
            self.autoApiInfo=AutoAPI.Info(self)
        if not hasattr(self.autoApiInfo,"expose"):
            self.autoApiInfo.expose=set()
        self.autoApiInfo.expose.update([
            "listall","listattribute","listdimension","listglobal","listvariable",
            "showglobal","showvariable","showattribute","showdimension","showall",
            "dimensionobject","dimensionarray",
            "getdimensionunits","getglobal","getattribute","getslab",
            "readScripGrid",
            ])
        self.cleardefault()

    def __call__ (self, id, *args, **kwargs):
        """Call a variable object with the given id. Exception if not found.
           Call the variable with the other arguments.
        """
# note defined here because this is the class all the dataset-type classes inherit
        v = self.variables.get(id)
        if v is None:
            try:
                if ( self.is_gridspec_grid_file() and
                     ( id=='' or id=='grid' or id=='gridspec' ) and
                     len(args)==0 and len(kwargs)==0
                     ):
                    return self.readg()
                else:
                    raise CDMSError, "No such variable or grid, " + id
            except ( AttributeError, TypeError ):
                raise CDMSError, "No such variable, " + id
        return v(*args, **kwargs)

    def __getitem__(self, key):
        """Implement f['varname'] for file/dataset f.
        """
        for d in [self.variables, self.axes, self.grids]:
            if d.has_key(key):
                result = d[key]
                break
        else:
            result = None
        return result

    def _v(self, vname):
        "Get the variable vname as a file variable object."
        try:
            v = self.variables[vname]
        except KeyError:
            raise CDMSError, "No variable named " + vname + " in file " + \
                  self.id
        return v

    def default_variable (self, vname):
        "Set the default variable name."
        self.__dict__['default_variable_name'] = vname

    def cleardefault (self):
        "Clear the default variable name."
        self.default_variable("no_default_variable_name_specified")
    
    def listall (self, vname=None, all=None):
        """Get info about data from the file.
        :::
        Options:::
        vname :: (str/None) (None) variable name
        all :: (None/True/False/int) (None) include axes information
        :::
        """
        if vname is None: vname = self.default_variable_name
        try:
            m = numpy.get_printoptions()['threshold']
            result = []
            result.append('*** Description of slab %s in file %s***' % \
                          (vname, self.id))
            result.append('Name: ' + vname)
            v = self._v(vname)
            a = v.attributes
            for x, y in a.items():
                result.append(x + ": " + str(y))
            d = v.getDomain()
            nd = 0
            for x in d:
                nd = nd + 1
                result.append('** Dimension ' + str(nd) + ' **')
                axis = x[0]
                n = numpy.ma.size(axis[:])
                numpy.set_printoptions(edgeitems=n)
                result.append('Name: ' + axis.id)
                result.append('Units: ' + axis.units)
                result.append('Length: ' + str(n))
                result.append('First: ' + str(axis[0]))
                result.append('Last: ' + str(axis[-1]))
                if all:
                    result.append(str(self.dimensionarray(axis.id, vname)))
            result.append ('*** End of description of %s ***' %vname)
            return result
        finally:
            numpy.set_printoptions (threshold=m)
        
    def listattribute (self, vname=None):
        """Get attributes of data from the file.
        :::
        Options:::
        vname :: (str/None) (None) variable name
        :::
        """
        if vname is None: vname = self.default_variable_name
        v = self._v(vname)
        return v.attributes.keys()

    def listdimension (self, vname=None):
        """Return a list of the dimension names associated with a variable.
           If no argument, return the file.axes.keys()
        :::
        Options:::
        vname :: (str/None) (None) variable name
        :::
        """
        if vname is None: 
            return self.axes.keys()
        v = self._v(vname)
        d = v.getDomain()
        x = map(lambda n: n[0], d)
        return map (lambda n: getattr(n, 'id'), x)

    def listglobal (self):
        """Returns a list of the global attributes in the file.
        :::
        """
        return self.attributes.keys()

    def listvariable (self):
        """Return a list of the variables in the file.
        :::
        """
        return self.variables.keys()

    listvariables = listvariable

    def showglobal (self, device=None):
        """Show the global attributes in the file.
        :::
        Options:::
        device :: (None/file) (None) output device
        :::
        """
        if device is None: device=sys.stdout
        device.write("Global attributes in file ")
        device.write(self.id)
        device.write(":\n")
        device.write(str(self.listglobal()))
        device.write("\n")

    def showvariable (self, device=None):
        """Show the variables in the file.
        :::
        Options:::
        device :: (None/file) (None) output device
        :::
        """
        if device is None: device=sys.stdout
        device.write("Variables in file ")
        device.write(self.id)
        device.write(":\n")
        device.write(str(self.listvariable()))
        device.write("\n")

    def showattribute (self, vname=None, device=None):
        """Show the attributes of vname.
        :::
        Options:::
        vname :: (str/None) (None) variable name
        device :: (None/file) (None) output device
        :::
        """
        if device is None: device=sys.stdout
        if vname is None: vname = self.default_variable_name
        device.write("Attributes of ")
        device.write(vname)
        device.write(" in file ")
        device.write(self.id)
        device.write(":\n")
        device.write(str(self.listattribute(vname)))
        device.write("\n")
        
    def showdimension (self, vname=None, device=None):
        """Show the dimension names associated with a variable.
        :::
        Options:::
        vname :: (str/None) (None) variable name
        device :: (None/file) (None) output device
        :::
        """
        if device is None: device=sys.stdout
        if vname is None: vname = self.default_variable_name
        device.write("Dimension names of ")
        device.write(vname)
        device.write(" in file ")
        device.write(self.id)
        device.write(":\n")
        device.write(str(self.listdimension(vname)))
        device.write("\n")
        
    def showall (self, vname=None, all=None, device=None):
        """Show a full description of the variable.
        :::
        Options:::
        vname :: (str/None) (None) variable name
        all :: (None/True/False/int) (None) include axes information
        device :: (None/file) (None) output device
        :::
        """
        if device is None: device=sys.stdout
        if vname is None: vname = self.default_variable_name
        alist = self.listall(vname, all=all)
        device.write(string.join(alist, "\n"))
        device.write("\n")

    def dimensionobject (self, dname, vname=None):
        """CDMS axis object for the dimension named dname.
        :::
        Options:::
        vname :: (str/None) (None) variable name
        :::
        Input:::
        dname :: (str) (0) dimension name
        :::
        Output:::
        axis :: (cdms2.axis.FileAxis) (0) file axis whose id is vname
        :::
        """
        if vname is None: 
            try:
                return self.axes[dname]
            except KeyError:
                raise CDMSError, "No axis named " + dname + " in file " +\
                                self.id + "."
        else:
            v = self._v(vname)
            d = v.getDomain()
            for x in d:
                if x[0].id == dname:
                    return x[0]
            else:
                raise CDMSError, vname + " has no axis named " + dname + \
                                " in file " + self.id + "."
        
    def dimensionarray (self, dname, vname=None):
        """Values of the dimension named dname.
        :::
        Options:::
        vname :: (str/None) (None) variable name
        :::
        Input:::
        dname :: (str) (0) dimension name
        :::
        Output:::
        axisvalues :: (numpy.ndarray) (0) array with values of axis whose id is vname
        :::
        """
        return self.dimensionobject(dname, vname).getValue()
    
    def getdimensionunits (self, dname, vname=None):
        """Get the units for the given dimension.
        :::
        Options:::
        vname :: (str/None) (None) variable name
        :::
        Input:::
        dname :: (str) (0) dimension name
        :::
        Output:::
        units :: (str) (0) units of axis whose id is vname
        :::
        """
        x = self.dimensionobject(dname, vname)
        return x.units

    def getglobal (self, attribute):
        """Get the value of the global attribute.
        :::
        Input:::
        attribute :: (str) (0) global attribute name
        :::
        Output:::
        attribute_value :: (str/int/float/numpy.ndarray) (0) value of requested global attribute
        :::
        """
        try:
            return self.attributes[attribute]
        except KeyError:
            return None
    
    def getattribute (self, vname, attribute):
        """Get the value of attribute for variable vname
        :::
        Input:::
        vname :: (str/None) (0) variable name
        attribute :: (str) (1) attribute name
        :::
        Output:::
        attribute_value :: (str/int/float/numpy.ndarray) (0) value of requested attribute
        :::
        """
        v = self._v(vname)
        return getattr(v, attribute)
            
    def getslab (self, vname, *args,**keys):
        """getslab('name', arg1, arg2, ....) returns a cdms variable
           containing the data.

           Arguments for each dimension can be:
              (1) : or None -- selected entire dimension
              (2) Ellipsis -- select entire dimensions between the ones given.
              (3) a pair of successive arguments giving an interval in
                  world coordinates.
              (4) a cdms-style tuple of world coordinates e.g. (start, stop, 'cc')
        :::
        Options:::
        args :: (*tuple/*cdms2.selectors.Selector) () tuple of type (val1,val2,'cob') for any given dimension or cdms selector
        :::
        Keys:::
        squeeze :: (int/True/False) (0) squeezes (removes) dimensions of length 1
        order :: (str) ('...') reorder the dimensions, can use numbers or xyzt or dim names in between paranthesis
        raw :: (int/True/False) (0) return a numpy.ma instead of a transient variable
        grid :: (cdms2.grid.AbstractGrid) (None) regrid the result to the grid passed
        :::
        Input:::
        vname :: (str/None) (0) variable name
        :::
        Output:::
        variable :: (cdms2.tvariable.TransientVariable) (0) variable requested
        :::
        """
        nargs = len(args)
        v = self._v(vname)
        if nargs == 0:
           return v.subRegion()
# note CDMS treats None as a colon in getRegion and mapInterval
        ndims = v.rank()
        cdms_args = [':'] * ndims 
        i = 0
        idim = 0
        ne = 0
        while i < nargs:
            if not (idim < ndims):
                raise CDMSError, "Too many arguments to getslab."
            x = args[i]
            if x == ':' or x == None:
                i = i + 1
                idim = idim + 1
                continue
            elif x == Ellipsis:
                if ne: raise CDMSError, "Only one ellipsis allowed."
                idim = ndims - (nargs - i - 1)
                i = i + 1
                ne = 1
            elif type(x) == types.TupleType:
                cdms_args[idim] = x
                idim = idim + 1
                i = i + 1
            else:
                if not ((i+1) < nargs):
                    raise CDMSError, "Arguments to getslab not paired properly."
                low = float(x)
                high = float(args[i+1])
                cdms_args[idim] = (low, high, 'cc')
                idim = idim + 1
                i = i + 2
        sq = keys.get('squeeze', 0)
        result = apply(v.subRegion, tuple(cdms_args), {'squeeze':sq})
        result.parent = self
        result.id = vname
        return result

    def readScripGrid(self, whichGrid="destination", checkGrid=1):
        """Read a SCRIP curvilinear or generic grid from the dataset.
        The dataset can be a SCRIP grid file or mapping file. If a mapping file,
        'whichGrid' chooses the grid to read, either "source" or "destination".
        If 'checkGrid' is 1 (default), the grid cells are checked for convexity,
        and 'repaired' if necessary.
        Returns the grid object.
        :::
        Options:::
        whichGrid :: (str) ('destination') grid to read
        checkGrid (int) (1) if 1 the grid cells are checked for convexity
        :::
        Output:::
        grid :: (cdms2.hgrid.TransientCurveGrid/cdms2.gengrid.TransientGenericGrid) (0) variable requested
        :::
        """
        
        import hgrid, gengrid

        # Grid file
        if 'grid_dims' in self.variables.keys():
            dims = self('grid_dims')
            whichType = "grid"

        # Destination grid from mapping file
        elif whichGrid=="destination":
            dims = self('dst_grid_dims')
            whichType = "mapping"

        # Source grid from mapping file
        else:
            dims = self('src_grid_dims')
            whichType = "mapping"

        if len(dims)==2:
            result = hgrid.readScripCurveGrid(self, dims, whichType, whichGrid)
        elif len(dims)==1:
            result = gengrid.readScripGenericGrid(self, dims, whichType, whichGrid)
        else:
            raise CDMSError, "Grid rank must be 1 or 2, found: %d"%len(dims)

        if checkGrid==1:
            nonConvexCells = result.checkConvex()
            result.fixCutCells(nonConvexCells)

        return result

