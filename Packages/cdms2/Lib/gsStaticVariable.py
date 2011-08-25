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
from cdms2.hgrid import AbstractCurveGrid, TransientCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis

try:
    from pycf import libCFConfig as libcf
except:
    raise ImportError, 'Error: could not import pycf'

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

    # Define the axes, verifying the lon and lat grids
    iaxis = TransientVirtualAxis("i", ni)
    jaxis = TransientVirtualAxis("j", nj)

    lonstr = 'lon'
    latstr = 'lat'

    if re.search(lonstr, x.standard_name): lon = x
    if re.search(lonstr, y.standard_name): lon = y
    if re.search(latstr, x.standard_name): lat = x
    if re.search(latstr, y.standard_name): lat = y

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
    Static variable extending over multiple grid files
    """

    def __init__(self, HostObj, varName, isFileVariable = False):
        """
        Constructor
        @param HostObj host object
        @param varName variable name
        """

        self.varName = varName
        self.ngrids = HostObj.ngrids

        self.vars = []
        if self.ngrids > 0:
            self.vars = [None]*self.ngrids

        for gfindx in range(self.ngrids):

            # name of the file containing the data on tile gfindx
            fName = HostObj.statVars[varName][gfindx]

            # name of the file containing coordinate data
            gFName = HostObj.gridFilenames[gfindx]

            fh = cdms2.open(fName, HostObj = HostObj)
            gh = cdms2.open(gFName)

            # alternatively vr = fh[varName]
            if isFileVariable: vr = fh[varName]
            else: vr = fh(varName)

            if isFileVariable:
                # Bracket operator - fileVariable
                self.vars[gfindx] = vr
            else:
                # Parenthetical operator - Transient Variable routine
                #vr.gridFilename = gFName
                vr.gridIndex    = gfindx

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
                self.vars[gfindx] = var

    def __getitem__(self, gfindx):
        """
        Data accessor
        @param gfindx grid file index
        @return variable at gfindx
        """
        return self.vars[gfindx]

    def __setitem__(self, gfindx, vals):
        """
        Data setter
        @param gfindx grid file indexer
        @param vals values to set
        """
        self.vars[gfindx] = vals

    def shape(self, gfindx):
        """
        Return the shape in the format (n0, n1, ...) for a given grid index
        @param gfindx grid file index
        @return result
        """
        return self.vars[gfindx].shape

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

    def __repr__(self):
        res = ""
        for gfindx in range(len(self.vars)):
            res += (" grid %d: " % gfindx) + repr(self.vars[gfindx])
        return res

def test():
    pass

if __name__ == '__main__': test()
 
