#/usr/bin/env python

"""
A variable-like object extending over multiple tiles and time slices
Dave Kindig and Alex Pletzer, Tech-X Corp. (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""

import operator
import cdms2
from cdms2.MV2 import concatenate
from cdms2.gsStaticVariable import StaticVariable
from cdms2.error import CDMSError
from cdms2.hgrid import AbstractCurveGrid, TransientCurveGrid, FileCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
from cdms2.Cdunif import CdunifFile
from cdms2.coord import FileAxis2D
from cdms2.gengrid import FileGenericGrid
from cdms2.fvariable import FileVariable
from cdms2.axis import FileAxis

class TimeVariable:
    """
    Constructor class for time variables
    """
    def __init__(self, TimeObj, HostObj, varName):
        """
        Constructor
        @param HostObj host object 
        @param varName variable name
        """
        TimeObj.id = varName
        TimeObj.ntimeSlices = HostObj.ntimeSlices

        TimeObj.vars = []

    def __getitem__(self, indices):
        """
        Data accessor
        @param gridIndex the grid index < HostObj.ngrids
        @param timeIndex the time slice index < HostObj.ntimeslices
        @return variable at gridIndex
        """
        from types import *
        if type(indices) is IntType:
            return self.vars[indices]
        elif type(indices) is SliceType:
            # Return the given grids.
            pass
        elif len(indices) > 2: 
            print "%s\n%s" % \
            ("variable[gridIndex] OR variable[gridIndex, timeIndex]",  \
             "variable[gridIndex, timeIndexSlice] to aggregate a grid in time")
            raise CDMSError, "Check number of indices"
        elif len(indices) == 2:
            gridIndex = indices[0]
            timeIndex = indices[1]

        # May need to iterate over the grids. Maybe the user should do this?
        if type(timeIndex) is SliceType:
            # Aggregate in time
            # self.aggregateInTime(self, gridIndex, timeIndex)
            pass
        elif type(timeIndex) is IntType:
            return self.vars[gridIndex][timeIndex]

    def __call__(self, gridIndex, timeIndex):
        """
        Data accessor
        @param gridIndex the grid index < HostObj.ngrids
        @param timeIndex the time slice index < HostObj.ntimeslices
        @return variable at gridIndex
        """
        return self.vars[gridIndex][timeIndex]

    def __setitem__(self, indices, vals):
        """
        Data setter
        @param indices list or tuple (gridIndex, timeIndex)
        @param vals values to set
        """
        if len(indices) != 2: 
            raise 'indices must be a two item list or tuple (gridIndex, timeIndex)'
        self.vars[gridIndex][timeIndex] = vals

    def len(self, gridIndex = None, timeIndex = None):
        """
        Length aka ngrids
        @param gridIndex the grid index < HostObj.ngrids
        @param timeIndex the time slice index < HostObj.ntimeslices
        @return a length dependent upon the input indices
        """
        # Return a length of some sort
        if gridIndex is None: 
            return len(self.vars)
        else:
            if timeIndex is None: return len(self.vars[gridIndex])
            else: return len(self.vars[gridIndex][timeIndex])

    def shape(self, gridIndex, timeIndex):
        """
        Return the shape in the format (n0, n1, ...) for a given grid index
        @param gridIndex the grid index < HostObj.ngrids
        @param timeIndex the time slice index < HostObj.ntimeslices
        @return result
        """
        return self.vars[gridIndex][timeIndex].shape

    def size(self):
        """
        Return the total number of elements for the whole grid
        @return number of elements
        """
        # adding the size of each tile
        vsize = 0
        for l in self.vars:
            for ll in l:
                vsize = vsize + len(l) + len(ll) + ll.size
        return vsize

    def typecode(self):
        """
        Return the type of the data
        @return type
        """
        v = self.vars[0][0]
        if v:
            return self.vars[0][0].typecode()
        return None

    def __repr__(self):
        res = ""
        if not hasattr(self, 'vars'): 
            res = "< %s >" % ("gsTimeVariable")
        else:
            for gridIndex in range(len(self.vars)):
                res += ("grid %d: " % gridIndex) + repr(self.vars[gridIndex])
            res = "<%s, %s>" % (self._repr_string, res)
        return res

    def aggregateInTime(self, grid, gridIndex, timeIndex):
        """
        Aggregate a time variable per grid.
        timeVariable[grid]
        @return timeVariable
        """
        pass
        for gridIndex in range(HostObj.ngrids):

            # Create the horizontal curvilinear grid.
            # But how do I add the time grid? I don't know it yet.
            # It is known after looping over the time files for a given
            # variable
            gFName = HostObj.gridFilenames[gridIndex]
            
            for timeIndex in range(HostObj.ntimeSlices):
                fName = HostObj.timeDepVars[varName][timeIndex][gridIndex]
                fh = cdms2.open(fName)
                print isFileVariable
                if isFileVariable:
                    var = fh[varName]
                    print type(var)
                    if timeIndex == 0: 
                        new = var
                    else: 
                        tmp = concatenate((new, var))
                        new = tmp
                else:
                    # TransientVariable
                    var = fh(varName, **slicekwargs)

                    # Attach the grid to the variable
                    grid = cdms2.gsStaticVariable.createTransientGrid(gFName, \
                                         var.attributes['coordinates'])
                    axis0 = var.getAxis(0)
                    gridaxes = grid.getAxisList()
                    axes = [axis0, gridaxes[0], gridaxes[1]]
                    atts = dict(var.attributes)
                    atts.update(fh.attributes)

                    # Create cdms2 transient variable
                    if timeIndex == 0:
                        new = cdms2.createVariable(var, 
                                    axes = axes, 
                                    grid = grid, 
                                    attributes = atts, 
                                    id = var.standard_name)
                    else:
                        tmp = concatenate((new, var))
                        axis0 = tmp.getAxis(0)
                        gridaxes = grid.getAxisList()
                        axes = [axis0, gridaxes[0], gridaxes[1]]

                        # Recreate the variable with all the decorations
                        new  = cdms2.createVariable(tmp, 
                                    axes = axes, 
                                    grid = grid, 
                                    attributes = atts, 
                                    id = var.standard_name)
                        
                    fh.close()

            # Add the variable to the index
            print type(new)
            self.vars[gridIndex] = new

class TimeTransientVariable(TimeVariable):
    def __init__(self, HostObj, varName, **slicekwargs):
        """
        Constructor
        @param HostObj host object 
        @param varName variable name
        @param slicekwargs eg lon=(-180,180), lat=(-90,90), time=5
                           cf Packages/cdms2/Lib/cudsinterface.py for 
                           a list of keywords
        """
        
        TimeVariable(self, HostObj, varName)

        kwargs = {}
        for k in slicekwargs.keys():
            kwargs[k.lower()] = slicekwargs[k]

        # time dependent variable. Create a list of list. One list for each
        # grid populated by a list for each time file.
        if ('time' in kwargs.keys() and len(slicekwargs) <= 1) or \
                len(slicekwargs) == 0:
            for gridIndex in range(HostObj.ngrids):

                gFName = HostObj.gridFilenames[gridIndex]

                for timeIndex in range(HostObj.ntimeSlices):

                    fName = HostObj.timeDepVars[varName][timeIndex][gridIndex]
                    fh = cdms2.open(fName, HostObj=HostObj)

                    # TransientVariable
                    var = fh(varName, **slicekwargs)

                    # Attach the grid to the variable
                    grid = cdms2.gsStaticVariable.createTransientGrid(gFName, \
                                         var.attributes['coordinates'])
                    axis0 = var.getAxis(0)
                    gridaxes = grid.getAxisList()
                    axes = [axis0, gridaxes[0], gridaxes[1]]
                    atts = dict(var.attributes)
                    atts.update(fh.attributes)

                    # Create cdms2 transient variable
                    tmp = cdms2.createVariable(var, 
                                axes = axes, 
                                grid = grid, 
                                attributes = atts, 
                                id = var.standard_name)
                    if timeIndex == 0:
                        new = [tmp]
                    else:
                        new.append(tmp)
                    fh.close()

                # Add the variable to the index
                self.vars.append(new)

            print len(self.vars)
        self._repr_string = "TimeTransientVariable"

class TimeFileVariable(StaticVariable):
    def __init__(self, HostObj, varName):
        """
        Create a list of file variable with grid attached
        @param HostObj The host object opened by gsHost
        @param varName the variable name to be returned
        """

        TimeVariable(self, HostObj, varName)
        mode = HostObj.mode

        for gridIndex in range(HostObj.ngrids):

            # Get the filenames
            gn = HostObj.gridFilenames[gridIndex]
            g = CdunifFile(gn, mode)

            for timeIndex in range(self.ntimeSlices):

                # Open the files
                fn = HostObj.timeDepVars[varName][gridIndex][timeIndex]
                f = cdms2.open(fn, mode)   # Need f and u because they serve slightly different purposes
                u = CdunifFile(fn, mode)   # f.axes exists while axes is not a part of u
#                u.variables[varName].gridIndex = gridIndex

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
#                grid = FileGenericGrid(lat, lon, gridname, parent = f, maskvar = None)
                grid = FileCurveGrid(lat, lon, gridname, parent = f, maskvar = None)
                f.variables[varName]._grid_ = grid
                self.vars[gridIndex] = f.variables[varName]
        self._repr_string = "TimeFileVariable"


###################################################################

def test():
    pass

if __name__ == '__main__': test()

