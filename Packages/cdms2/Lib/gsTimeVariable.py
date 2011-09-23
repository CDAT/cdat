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
from cdms2.tvariable import TransientVariable
from cdms2.error import CDMSError
from cdms2.hgrid import AbstractCurveGrid, TransientCurveGrid, FileCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
from cdms2.Cdunif import CdunifFile
from cdms2.coord import FileAxis2D
from cdms2.gengrid import FileGenericGrid
from cdms2.fvariable import FileVariable
from cdms2.axis import FileAxis

#class TimeVariable:
#    """
#    Constructor class for time variables
#    """
#    def __init__(self, TimeObj, HostObj, varName):
#        """
#        Constructor
#        @param HostObj host object 
#        @param varName variable name
#        """
#        TimeObj.id = varName
#        TimeObj.vars = []
#
#    def __getitem__(self, indices, *args, **slicekwargs):
#        """
#        Data accessor
#        @param gridIndex the grid index < HostObj.nGrids
#        @param timeFileIndex the time slice index < HostObj.nTimeSliceFiles
#        @return variable at gridIndex
#        """
#        import types
#
#        def getVariable(var, *args):
#            """
#            args is a list of indices
#            """
#            pass
#
#        if len(slicekwargs.keys()) !=0:
#            print 'up'
#
#        if type(indices) is types.IntType:
#            return self.vars[indices]
#        elif type(indices) is types.TupleType:
#            if type(indices[0]) is types.SliceType:
#                raise CDMSError, "Slices not allowed for grid index"
#                # Return the given grids.
#                pass
#            elif len(indices) == 2:
#                gridIndex = indices[0]
#                timeFileIndex = indices[1]
#                if isinstance(timeFileIndex, int):
#                    return self.vars[gridIndex][timeFileIndex]
#                elif type(timeFileIndex) is types.SliceType:
#                    # Aggregate in time
#                    # self.aggregateInTime(self, gridIndex, timeFileIndex)
#                    pass
#                    # return aggregation
#                elif type(timeFileIndex) is types.EllipsisType:
#                    # We must aggregate in time before returning 
#                    pass
#                else:
#                    raise CDMSError, "Wrong type for timeFileIndex"
#
###############################################################################
#            # Below here, we must aggregate in time before returning 
###############################################################################
#
#            elif len(indices) == 3:
#                gridIndex = indices[0]
#                timeFileIndex = indices[1]
#                latsIndex = indices[2]
#                # Check into time and space slices using indices
#                # Latitude
#                pass
#            elif len(indices) == 4:
#                gridIndex = indices[0]
#                timeFileIndex = indices[1]
#                latsIndex = indices[2]
#                latsIndex = indices[3]
#                # Check into time and space slices using indices
#                # Latitude
#                pass
#
#            # May need to iterate over the grids. Maybe the user should do this?
#
#    def call__(self, gridIndex, timeFileIndex):
#        """
#        Data accessor
#        @param gridIndex the grid index < HostObj.nGrids
#        @param timeFileIndex the time slice index < HostObj.nTimeSliceFiles
#        @return variable at gridIndex
#        """
#        return self.vars[gridIndex][timeFileIndex]
#
#    def __setitem__(self, indices, vals):
#        """
#        Data setter
#        @param indices list or tuple (gridIndex, timeFileIndex)
#        @param vals values to set
#        """
#        if len(indices) != 2: 
#            raise 'indices must be a two item list or tuple (gridIndex, timeFileIndex)'
#        self.vars[gridIndex][timeFileIndex] = vals
#
#    def __len__(self, gridIndex = None, timeFileIndex = None):
#        """
#        Length aka nGrids
#        @param gridIndex the grid index < HostObj.nGrids
#        @param timeFileIndex the time slice index < HostObj.nTimeSliceFiles
#        @return a length dependent upon the input indices
#        """
#        # Return a length of some sort
#        if gridIndex is None: 
#            return len(self.vars)
#        else:
#            if timeFileIndex is None: return len(self.vars[gridIndex])
#            else: return len(self.vars[gridIndex][timeFileIndex])
#
#    def shape(self, gridIndex, timeFileIndex):
#        """
#        Return the shape in the format (n0, n1, ...) for a given grid index
#        @param gridIndex the grid index < HostObj.nGrids
#        @param timeFileIndex the time slice index < HostObj.nTimeSliceFiles
#        @return result
#        """
#        return self.vars[gridIndex][timeFileIndex].shape
#
#    def size(self):
#        """
#        Return the total number of elements for the whole grid
#        @return number of elements
#        """
#        # adding the size of each tile
#        vsize = 0
#        for l in self.vars:
#            for ll in l:
#                vsize = vsize + len(l) + len(ll) + ll.size
#        return vsize
#
#    def typecode(self):
#        """
#        Return the type of the data
#        @return type
#        """
#        v = self.vars[0][0]
#        if v:
#            return self.vars[0][0].typecode()
#        return None
#
#    def __repr__(self):
#        res = ""
#        if not hasattr(self, 'vars'): 
#            res = "< %s >" % ("gsTimeVariable")
#        else:
#            for gridIndex in range(len(self.vars)):
#                res += ("grid %d: " % gridIndex) + repr(self.vars[gridIndex])
#            res = "<%s, %s>" % (self._repr_string, res)
#        return res
#
#    def aggregateInTime(self, gridIndex, timeFileSlice):
#        """
#        Aggregate a time variable per grid.
#        timeVariable[grid]
#        @param gridIndex grid index to be aggregated in time
#        @param timeFileTuple tuple of start and end file indices over which to concatenate
#        @return timeVariable
#        """
#        from cdms2.MV2 import concatenate
#        var = self[gridIndex]
#        if isinstance(timeFileSlice, slice):
#            newVar = concatenate(var[timeFileSlice])
#            return newVar
#        else:
#            raise CDMSError, 'Need slice over time indices'
#
#        print "###########"

class TimeAggregatedFileVariable:
    def __init__(self, gridIndex, listOfFVs, HostObj):
        """
        @param listOfFVs List of cdms2.FileVariable
        """
        self.fvs = listOfFVs
        self.gridIndex = gridIndex
        self.HostObj = HostObj

    def __getitem__(self, slc):
        """
        @param slc Integer, slice or tuple of slices. If tuple 0 is time
        """

        it = self.fvs[0].getAxisIndex("time")
        self.nTimeStepsPerFV = (self.fvs[0].shape)[it]

        if isinstance(slc, int):
            # return FileVariable
            return self.fvs[slc]
        elif isinstance(slc, tuple):
            # create TransientVariable
            # do we need to aggregate in time?
            if isinstance(slc[0], slice):
                itfStart = self.getTimeFileIndex(slc[0].start)
                itfEnd = self.getTimeFileIndex(slc[0].stop)
                tv = self.createTransientVariable(itfStart, itfEnd) # could be made more efficient
                return tv[slc]
            elif isinstance(slc[0], int):
                index = self.getTimeFileIndex(slc[0])

                # Get just the file needed for the index slice requested.
                tv = self.createTransientVariable(index, index+1)
                if slc[0] > self.nTimeStepsPerFV: returnIndex = slc[0] - self.nTimeStepsPerFV
                else: returnIndex = slc[0]
                return tv[returnIndex]
                
        elif isinstance(slc, slice):
            itfStart = self.getTimeFileIndex(slc.start)
            itfEnd = self.getTimeFileIndex(slc.stop)
            tv = self.createTransientVariable(itfStart, itfEnd) # could be made more efficient
            return tv[slc]

    def __call__(self, **kw):
        if kw.has_key('time'):
            itStart = self.getTimeIndexFromStr( kw['time'][0] )
            itEnd = self.getTimeIndexFromStr( kw['time'][1] )
            tv = self[itStart:itEnd, ...]
            return tv(kw)

    def __len__(self):
        return len(self.fvs)
    
    def getTimeFileIndex(self, index):
        """
        @param index The time index requested
        @param HostObj The host object contain the dimensions
        @return the file index for a given time index
        """
        HostObj = self.HostObj
        
        # Loop over the number of time slices per file for the given variable and the number of
        # time steps per file.

        for timeFileIndex in range(HostObj.nTimeSliceFiles):
            for timeSliceIndex in range(self.nTimeStepsPerFV):
                cind = timeFileIndex * HostObj.nTimeSliceFiles + timeSliceIndex
                # Return the timeFileIndex
                if cind == index: return timeFileIndex
        return HostObj.nTimeDataFiles

    def createTransientVariable(self, startTimeFileIndex, endTimeFileIndex):
        """
        @param startTimeFileIndex 
        """
#        HostObj = self.HostObj
#        gFName = HostObj.gridFilenames[self.gridIndex]
        firstTime = True
        rng = range(startTimeFileIndex, endTimeFileIndex)
        for i in rng:
            var = self.fvs[i][:]

            if firstTime:
                new = var
                firstTime = False
            else:
                tmp = concatenate((new, var))
                new = tmp

        return new

class TimeFileVariable:
    def __init__(self, HostObj, varName):
        """
        Create a list of file variable with grid attached
        @param HostObj The host object opened by gsHost
        @param varName the variable name to be returned
        """

        self.id = varName
        self.vars = []
        mode = HostObj.mode

        for gridIndex in range(HostObj.nGrids):

            # Get the filenames
            aa = HostObj.gridVars.keys()
            gn = HostObj.gridVars[aa[0]][gridIndex]
            g = CdunifFile(gn, mode)

            vars = []

            for timeFileIndex in range(HostObj.nTimeDataFiles):

                # Open the files
                fn = HostObj.timeDepVars[varName][gridIndex][timeFileIndex]
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
                gridname = ("grid%d_" % gridIndex) + "%dx%d" % lat.shape
#                grid = FileGenericGrid(lat, lon, gridname, parent = f, maskvar = None)
                grid = FileCurveGrid(lat, lon, gridname, parent = f, maskvar = None)
                f.variables[varName]._grid_ = grid
                vars.append(f.variables[varName])

            tafv = TimeAggregatedFileVariable(gridIndex, vars, HostObj)
            self.vars.append(tafv)

        self._repr_string = "TimeFileVariable"

    def __getitem__(self, gridIndex):
        """
        @param gridIndex gridIndex
        """
        return self.vars[gridIndex]
        
class TimeTransientVariable:
    def __init__(self, HostObj, varName, **slicekwargs):
        """
        Constructor
        @param HostObj host object 
        @param varName variable name
        @param slicekwargs eg lon=(-180,180), lat=(-90,90), time=5
                           cf Packages/cdms2/Lib/cudsinterface.py for 
                           a list of keywords
        """
        
#        TimeVariable(self, HostObj, varName)
        self.id = varName
        self.vars = []

        kwargs = {}
        for k in slicekwargs.keys():
            kwargs[k.lower()] = slicekwargs[k]

        # time dependent variable. Create a list of list. One list for each
        # grid populated by a list for each time file.
        if ('time' in kwargs.keys() and len(slicekwargs) <= 1) or \
                len(slicekwargs) == 0:
            for gridIndex in range(HostObj.nGrids):

                gFName = HostObj.gridFilenames[gridIndex]

                for timeFileIndex in range(HostObj.nTimeDataFiles):

                    fName = HostObj.timeDepVars[varName][gridIndex][timeFileIndex]
                    fh = cdms2.open(fName, HostObj=HostObj)

                    # TransientVariable
                    var = fh(varName, **slicekwargs)

                    # Attach the grid to the variable
                    grid = cdms2.gsStaticVariable.createTransientGrid(gFName, \
                                         var.attributes['coordinates'])
                    axis0 = var.getAxis(0)
                    gridaxes = grid.getAxisList()
                    axes = [axis0] + list(gridaxes)
                    atts = dict(var.attributes)
                    atts.update(fh.attributes)

                    # Create cdms2 transient variable
                    if timeFileIndex == 0:
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
#                        new.append(tmp)
                        new = cdms2.createVariable(tmp, 
                                axes = axes, 
                                grid = grid, 
                                attributes = atts, 
                                id = var.standard_name)
                    fh.close()

                # Add the variable to the index
                self.vars.append(new)

        self._repr_string = "TimeTransientVariable"


###################################################################

def test():
    pass

if __name__ == '__main__': test()

