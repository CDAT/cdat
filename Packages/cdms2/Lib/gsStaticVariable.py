#/usr/bin/env python

"""
A variable-like object extending over multiple tiles
Dave Kindig and Alex Pletzer, Tech-X Corp. (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""

import operator
import cdms2
import types
from cdms2.error import CDMSError
from cdms2.hgrid import AbstractCurveGrid, TransientCurveGrid, FileCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
from cdms2.Cdunif import CdunifFile
from cdms2.coord import FileAxis2D
from cdms2.gengrid import FileGenericGrid
from cdms2.fvariable import FileVariable
from cdms2.axis import FileAxis

try:
    from pycf import libCFConfig as libcf
except:
#    raise ImportError, 'Error: could not import pycf'
    pass

def createTransientGrid(gFName, coordinates):
    """
    Return the coordinate data associated with variable.
    @param gName The grid_filename
    @param coordinates The coordinates attribute from the variable to be created
    @return grid a cdms2.hgrid.AbstractCurveGrid object
    """
    import re

    fh = cdms2.open(gFName)
    gridid = None
    if libcf.CF_GRIDNAME in fh.attributes.keys(): 
        gridid = getattr(fh, libcf.CF_GRIDNAME)
    xn, yn = coordinates.split()

    x = fh(xn)
    y = fh(yn)

    # Get the dimensions
    xdim = x.shape
    ydim = y.shape

    if xdim != ydim: 
        raise CDMSError, "Dimension of coordinates grids don't match"

    ni = xdim[1]
    nj = xdim[0]

    lonstr = 'lon'
    latstr = 'lat'

    if re.search(lonstr, x.standard_name): lon = x
    if re.search(lonstr, y.standard_name): lon = y
    if re.search(latstr, x.standard_name): lat = x
    if re.search(latstr, y.standard_name): lat = y

    # Define the axes, verifying the lon and lat grids
    iaxis = TransientVirtualAxis("i", ni)
    jaxis = TransientVirtualAxis("j", nj)

    lataxis = TransientAxis2D(lat, 
                   axes=(iaxis, jaxis), 
                   attributes={'units': lat.units}, 
                   id=lat.standard_name)
    lonaxis = TransientAxis2D(lon, 
                   axes=(iaxis, jaxis), 
                   attributes={'units': lon.units}, 
                   id=lon.standard_name)

    # Define the combined grid
    grid = TransientCurveGrid(lataxis, lonaxis, id=gridid)
    return grid

class StaticVariable:
    """
    Constructor
    """
    def __init__(self, StaticVariable, hostObj, varName):
        """
        Constructor - Contains methods applicable to both file and transient static variables
        @param StaticVariable A generic static variable (Either File or Transient)
        @param hostObj The host file object
        @param varName for the id
        """
        StaticVariable.id     = varName
        StaticVariable.nGrids = hostObj.nGrids

        StaticVariable.vars = []
        if StaticVariable.nGrids > 0:
            StaticVariable.vars = [None]*StaticVariable.nGrids

    def __getitem__(self, gridIndex):
        """
        Data accessor
        @param gridIndex grid file index
        @return variable at gridIndex
        """
        return self.vars[gridIndex]

    def __call__(self, gridIndex):
        """
        Data accessor
        @param gridIndex grid file index
        @return variable at gridIndex
        """
        return self.vars[gridIndex]

    def __setitem__(self, gridIndex, vals):
        """
        Data setter
        @param gridIndex grid file indexer
        @param vals values to set
        """
        self.vars[gridIndex] = vals

    def len(self):
        """
        Length aka nGrids
        """
        return len(self.vars)

    def shape(self, gridIndex):
        """
        Return the shape in the format (n0, n1, ...) for a given grid index
        @param gridIndex grid file index
        @return result
        """
        return self.vars[gridIndex].shape

    def size(self):
        """
        Return the total number of elements for the whole grid
        @return number of elements
        """
        # adding the size of each tile
        return reduce(operator.add, [v.size for v in self.vars])

    def typecode(self):
        """
        Return the type of the data
        @return type
        """
        v = self.vars[0]
        if v:
            return self.vars[0].typecode()
        return None

class StaticFileVariable(StaticVariable):
    """
    Static variable extending over multiple grid files
    """
    def __init__(self, hostObj, varName):
        """
        Create a list of file variable with grid attached
        @param hostObj The host object opened by gsHost
        @param varName the variable name to be returned
        """

        StaticVariable(self, hostObj, varName)
        mode = hostObj.mode
        gridFilenames = hostObj.getGridFilenames()

        for gridIndex in range(self.nGrids):

            # Get the filenames
            fn = hostObj.statVars[varName][gridIndex]
            gn = gridFilenames[gridIndex]

            # Open the files
            f = cdms2.open(fn, mode)   # Need f and u because they serve slightly different purposes
            u = CdunifFile(fn, mode)   # f.axes exists while axes is not a part of u
#            u.variables[varName].gridIndex = gridIndex
            g = CdunifFile(gn, mode)

            # Turn the coordinates into a list
            if hasattr(u.variables[varName], "coordinates"):
                coords = u.variables[varName].coordinates.split()

            # Get lists of 1D and auxiliary coordinate axes
            coords1d = f._convention_.getAxisIds(u.variables)
            coordsaux = f._convention_.getAxisAuxIds(u.variables, coords1d)

            # Convert the variable into a FileVariable
            f.variables[varName] = FileVariable(f, varName, u.variables[varName])

            # Add the coordinates to the file
            for coord in coords:
                f.variables[coord] = g.variables[coord]
                f.variables[coord] = FileAxis2D(f, coord, g.variables[coord])
            
            # Build the axes
            for key in f.axes.keys():
                f.axes[key] = FileAxis(f, key, None)

            # Set the boundaries
            for coord in coords:
                bounds = f._convention_.getVariableBounds(f, f.variables[coord])
                f.variables[coord].setBounds(bounds)

            # Initialize the domain
            for var in f.variables.values():
                var.initDomain(f.axes)

            # Add the grid
            gridkey, lat, lon = f.variables[varName].generateGridkey(f._convention_, f.variables)
            gridname = "grid_%dx%d" % lat.shape
#            grid = FileGenericGrid(lat, lon, gridname, parent = f, maskvar = None)
            grid = FileCurveGrid(lat, lon, gridname, parent = f, maskvar = None)
            f.variables[varName]._grid_ = grid
            self.vars[gridIndex] = f.variables[varName]
        self._repr_string = "StaticFileVariable"

    def listall(self, all = None):
        """
        Gain access to cdms2 listall method. Requires a StaticFileVariable
        @param all
        @returns list
        """
        return self[0].listall(all = all)

class StaticTransientVariable(StaticVariable):
    """
    Static variable extending over multiple grid files
    """
    def __init__(self, hostObj, varName):
        """
        Constructor
        @param hostObj host object
        @param varName variable name
        """

        # Inititialize the variable
        StaticVariable(self, hostObj, varName)
        gridFilenames = hostObj.getGridFilenames()

        for gridIndex in range(self.nGrids):

            # name of the file containing the data on tile gridIndex
            fName = hostObj.statVars[varName][gridIndex]

            # name of the file containing coordinate data
            gFName = gridFilenames[gridIndex]

            fh = cdms2.open(fName, hostObj = hostObj)
            gh = cdms2.open(gFName)

            vr = fh(varName)
            vr.gridIndex    = gridIndex

            grid = None
            if 'coordinates' in vr.attributes.keys():
                grid = createTransientGrid(gFName, vr.attributes['coordinates'])
            atts = dict(vr.attributes)
            atts.update(gh.attributes)
            if libcf.CF_GRIDNAME in fh.attributes.keys():
                atts[libcf.CF_GRIDNAME] = getattr(fh, libcf.CF_GRIDNAME)

            # Create the variable
            if grid:
                var = cdms2.createVariable(vr, 
                                axes = grid.getAxisList(), 
                                grid = grid, 
                                attributes = atts, 
                                id = vr.standard_name)
            else: 
                var = vr
            self.vars[gridIndex] = var
        self._repr_string = "StaticTransientVariable"

def test():
    pass

if __name__ == '__main__': test()
 
