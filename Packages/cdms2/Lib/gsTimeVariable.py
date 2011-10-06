#/usr/bin/env python

"""
A variable-like object extending over multiple tiles and time slices
Dave Kindig and Alex Pletzer, Tech-X Corp. (2011)
This code is provided with the hope that it will be useful.
No guarantee is provided whatsoever. Use at your own risk.
"""

import operator
import cdtime
import cdms2
from cdms2.MV2 import concatenate as MV2concatenate
from cdms2.gsStaticVariable import StaticVariable
from cdms2.tvariable import TransientVariable
from cdms2.error import CDMSError
from cdms2.hgrid import AbstractCurveGrid, TransientCurveGrid, FileCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
from cdms2.Cdunif import CdunifFile
from cdms2.coord import FileAxis2D
from cdms2.gengrid import FileGenericGrid
from cdms2.fvariable import FileVariable
from cdms2.axis import FileAxis, TransientAxis
from cdms2.axis import concatenate as axisConcatenate

class TimeAggregatedFileVariable:
    """
    Constructor Class for aggregating a time dependant variable across files.
    """
    def __init__(self, gridIndex, listOfFVs, hostObj):
        """
        @param gridIndex Index of requested grid
        @param listOfFVs List of cdms2.FileVariable
        @param hostObj For access to constants
        """
        self.fvs = listOfFVs
        self.gridIndex = gridIndex
        self.hostObj = hostObj
        self.nTimeStepFiles = hostObj.nTimeSliceFiles * hostObj.nTimeDataFiles * hostObj.nGrids
        it = self.getTimeAxisIndex(self.fvs[0].getAxisList())
        self.nTimeStepsPerFile = (self.fvs[0].shape)[it]
        self.nTimeStepsPerVariable = hostObj.nTimeSliceFiles * self.nTimeStepsPerFile

    def __call__(self, *args, **kwargs):
        """
        @param *args cdms2 arguments
        @param kwargs cdms2 keywords
        @return sliced variable
        """

        subsetList = []
        for iFile in range(self.hostObj.nTimeSliceFiles):
            try:
                var = self.fvs[iFile](*args, **kwargs)
                subsetList.append(var)
            except Exception:
                pass

        newvar = self.createTransientVariableFromList(subsetList)

        return newvar

    def __getitem__(self, slc):
        """
        @param slc Integer, slice or tuple of slices. If tuple 0 is time
        @return sliced variable
        """
        
        if isinstance(slc, int):
            # return FileVariable
            return self.fvs[slc]
        elif isinstance(slc, tuple):
            # create TransientVariable
            # do we need to aggregate in time?
            nTSF = self.nTimeStepsPerFile
            axes = self.fvs[0].getAxisList()
            timeAxisIndex = self.getTimeAxisIndex(axes)
            if timeAxisIndex is None:
                CDMSError, "No time axis in :\n"  + axes
            if isinstance(slc[timeAxisIndex], slice):
                (fileInds, timeStepInds) = self.getTimeFileIndex(slc[timeAxisIndex])
                tv = self.createTransientVariableFromIndices(fileInds, timeStepInds)
                newslc = self.buildSlice(slc, tv.getAxisList())
                return tv[newslc]
            elif isinstance(slc[timeAxisIndex], int):
                fileIndex = slc[timeAxisIndex] / nTSF
                timeIndex = slc[timeAxisIndex] % nTSF

                # Get just the file needed for the index slice requested.
                tv = self.createTransientVariableFromIndices(fileIndex, timeIndex)
                newslc = self.buildSlice(slc, axes)
                return tv[newslc]

        elif isinstance(slc, slice):
            (fileInds, timeStepInds) = self.getTimeFileIndex(slc)
            tv = self.createTransientVariableFromIndices(fileInds, timeStepInds)
            return tv

    def __len__(self):
        return len(self.fvs)

    def getTimeFileIndex(self, timeslc):
        """
        @param index The time index requested
        @return the file index for a given time index
        """

        # Loop over the number of time slices per file for the given variable and
        # the number of time steps per file.

        nTSF = self.nTimeStepsPerFile
        nTSV = self.nTimeStepsPerVariable
        timI1 = []
        filI1 = []
        timI2 = []
        filI2 = []

        if timeslc.step is None: step = 1
        else: step = timeslc.step
        stop = timeslc.stop
        if timeslc.stop >= nTSV:
            stop = nTSV
        ii = [i / nTSF for i in range(timeslc.start, stop, step)]
        tt = [i % nTSF for i in range(timeslc.start, stop, step)]
        indx = 0
        for i in ii:
            if indx == 0:
                timI1.append(tt[indx])
                filI1.append(ii[indx])
            else:
                if ii[indx] == ii[indx-1]:
                    timI1.append(tt[indx])
                    filI1.append(ii[indx])
                else:
                    timI2.append(timI1)
                    filI2.append(filI1)
                    timI1 = []
                    filI1 = []
                    timI1.append(tt[indx])
                    filI1.append(ii[indx])
            indx += 1

        filI2.append(filI1)
        timI2.append(timI1)

        return filI2, timI2

    def getTimeAxisIndex(self, inAxes):
        """
        Get the index for the time index
        @param inAxes The axes list where we want to find the time index
        @return the index - None if time not found
        """
        for indx, axis in enumerate(inAxes):
            if axis.isTime(): return indx
            return None

    def buildSlice(self, inslc, inAxes):
        """
        Build a slice where the global time is either removed (single time step
        requested) OR the all requested times are returned. This is based on
        the new variables time shape.
        @param inslc The original slice
        @param inAxes The input axis to search for the time axis
        @return newslc New slice with no time dimension or the time axis slice is
                       (None, None, None)
        """
        newslc = []
        for cslc, axis in zip(inslc, inAxes):
            if axis.isTime():
                if type(cslc) is int:
                    # Omit slice - the new variable has only the shape of the grid.
                    continue
                else:
                    newslc.append(slice(None, None, None))
            else:
                newslc.append(cslc)
        return tuple(newslc)

    def buildAxes(self, timeAxis, inAxes):
        """
        Construct the time axis based on the current time index and insert into
        proper location in the axes list
        @param timeAxis The Current time axis
        @param inAxes The current axes
        @return new axes
        """
        axes = []
        for axis in inAxes:
            if axis.isTime():
                axes.append(timeAxis)
            else:
                axes.append(axis)
        return axes

    def createTransientVariableFromList(self, tvList):
        """
        Aggregate a sliced/subseted list of transient variables.
        @param tvlist List of subset/sliced transient variables.
        @return aggregated transient variable
        """

        outvar = tvList[0]
        if len(tvList) > 1:
            for varIndex in range(1, len(tvList)):
                new = MV2concatenate((outvar, tvList[varIndex]))
                outvar = new

        return outvar

    def createTransientVariableFromIndices(self, fileIndices, timeIndices):
        """
        Aggregate a time file variable. Start and End Indices use slice notation.
        @param fileIndices the file indices to aggregate across
        @param timeIndices which time steps with in each file
        @return aggregated time dep. variable. Has shape of full grid. 
                Subset the grid after exiting.
        """
        from numpy import reshape
        firsttime = True
        nTSF = self.nTimeStepsPerFile
        if type(fileIndices) is not int:
            for files, times in zip(fileIndices, timeIndices):
                for indx, file in enumerate(files):
                    # Should make these slices.
                    cvar = self.fvs[file][times[indx]]

                    grid = self.fvs[file].getGrid()
                    atts = cvar.attributes

                    # Insert the new time axis.
                    axisTime = self.fvs[file].getTime()
                    timeAxis = TransientAxis([file * nTSF + times[indx]],
                                              attributes = axisTime.attributes,
                                              id = axisTime.id)
                    axes = self.buildAxes(timeAxis, self.fvs[file].getAxisList())

                    # shape --> tm1.shape = (1, :, :)
                    tm1 = reshape(cvar, tuple([1] + list(cvar.shape)))

                    # Attach needed items
                    var = cdms2.createVariable(tm1,
                            axes = axes,
                            grid = grid,
                            attributes = atts,
                            id = cvar.standard_name)

                    # Create cdms2 transient variable
                    if firsttime:
                        new = var
                        firsttime = False
                    else:
                        # insert the new time axis.
                        taA = new.getTime()
                        newTime = axisConcatenate((taA, timeAxis),
                                                  attributes = axisTime.attributes,
                                                  id = axisTime.id)
                        axes = self.buildAxes(newTime, self.fvs[file].getAxisList())

                        tmp = MV2concatenate((new, var))
                        new = cdms2.createVariable(tmp,
                                axes = axes,
                                grid = grid,
                                attributes = atts,
                                id = cvar.standard_name)

        else:
            new = self.fvs[fileIndices][timeIndices]

        return new

class TimeFileVariable:
    """
    Construct an aggregated time dependant variable.
    """
    def __init__(self, hostObj, varName):
        """
        Create a list of file variable with grid attached
        @param hostObj The host object opened by gsHost
        @param varName the variable name to be returned
        """

        self.id = varName
        self.vars = []
        mode = hostObj.mode

        for gridIndex in range(hostObj.nGrids):

            # Get the filenames
            aa = hostObj.gridVars.keys()
            gn = hostObj.gridVars[aa[0]][gridIndex]
            g = CdunifFile(gn, mode)

            vars = []

            for timeFileIndex in range(hostObj.nTimeDataFiles):

                # Open the files
                fn = hostObj.timeVars[varName][gridIndex][timeFileIndex]
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

            tafv = TimeAggregatedFileVariable(gridIndex, vars, hostObj)
            self.vars.append(tafv)

        self._repr_string = "TimeFileVariable"

    def listall(self, all = None):
        """
        Gain access to cdms2 listall method. Requires a TimeFileVariable
        @param all
        @returns list
        """
        return self[0][0].listall(all = all)

    def showall(self, all = None, device = None):
        """
        Gain access to cdms2 showall method
        Requires a TimeFileVariable
        @param all
        @param device 
        @returns list
        """
        return self[0][0][:].showall(all = all, device = device)

    def __getitem__(self, gridIndex):
        """
        @param gridIndex gridIndex
        """
        return self.vars[gridIndex]

###############################################################################
############## DEPRECIATED - Testing required to fully remove #################
###############################################################################

class TimeTransientVariable:
    def __init__(self, hostObj, varName, **slicekwargs):
        """
        Constructor
        @param hostObj host object
        @param varName variable name
        @param slicekwargs eg lon=(-180,180), lat=(-90,90), time=5
                           cf Packages/cdms2/Lib/cudsinterface.py for
                           a list of keywords
        """

#        TimeVariable(self, hostObj, varName)
        self.id = varName
        self.vars = []

        gridFilenames = hostObj.getGridFilenames()

        kwargs = {}
        for k in slicekwargs.keys():
            kwargs[k.lower()] = slicekwargs[k]

        # time dependent variable. Create a list of list. One list for each
        # grid populated by a list for each time file.
        if ('time' in kwargs.keys() and len(slicekwargs) <= 1) or \
                len(slicekwargs) == 0:
            for gridIndex in range(hostObj.nGrids):

                gFName = gridFilenames[gridIndex]

                for timeFileIndex in range(hostObj.nTimeDataFiles):

                    fName = hostObj.timeDepVars[varName][gridIndex][timeFileIndex]
                    fh = cdms2.open(fName, hostObj=hostObj)

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
                        tmp =MV2concatenate((new, var))
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

