#/usr/bin/env python

"""
A variable-like object extending over multiple tiles
$Id: gsStatVar.py 1654 2011-01-18 22:11:06Z pletzer $
"""

import operator
import ctypes
import cdms2
import types
from cdms2.error import CDMSError
from cdms2.hgrid import AbstractCurveGrid, TransientCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
from pycf import libCFConfig as libcf

class GsStaticVariable(object):
    """
    Open a static variable.
    """

    def __init__(self, GsHost, varName, *speclist):
        """
        Constructor
        @param GsHost host object
        @param varName variable name
        @param speclist cudsinterface.py list for subsetting variables. 
                        Not implemented locally as of now.
        """
        self.varName = varName
        self.ngrids = GsHost.ngrids

        # If the requested variable has coordinates it has a grid
        hasCoordinates = False

        self.vars = []
        if self.ngrids > 0:
            self.vars = [None for i in range(self.ngrids)]

        for gfindx in range(self.ngrids):
            fName = GsHost.statVars[varName][gfindx]
            gFName = GsHost.gridFilenames[gfindx]
            fh = cdms2.open(fName)

            gh = cdms2.open(gFName)

            vr = fh(varName)
            fh.close()
            vr.gridFilename = gFName
            vr.gridIndex    = gfindx

            if 'coordinates' in vr.attributes.keys():
              grid = self.createGrid(gFName, vr.attributes['coordinates'])
              hasCoordinates = True
            atts = dict(vr.attributes)
            atts.update(gh.attributes)
            if libcf.CF_GRIDNAME in fh.attributes.keys():
              atts[libcf.CF_GRIDNAME] = getattr(fh, libcf.CF_GRIDNAME)

            # Create the variable
            if hasCoordinates:
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
        Return the shape in the format (n0, n1, ...)
        @param gfindx grid file index
        @return result
        """
        return self.vars[gfindx].shape

    def size(self):
        """
        Return the total number of elements
        @return number of elements
        """
        return reduce(operator.add, [v.size for v in self.vars])

    def createGrid(self, gFName, coordinates):
        """
        Return the coordinate data associated with variable.
        @param gName The grid_filename
        @param coordinates The coordinates attribute from the variable to be created
        @return grid a cdms2.hgrid.AbstractCurveGrid object
        """
        from re import search

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

        if xdim != ydim: CDMSError, "Dimension of coordinates grids don't match"

        ni = xdim[1]
        nj = xdim[0]

        # Define the axes, verifying the lon and lat grids
        iaxis = TransientVirtualAxis("i",ni)
        jaxis = TransientVirtualAxis("j",nj)

        lonstr = 'lon'
        latstr = 'lat'

        if search(lonstr, x.standard_name): lon = x
        if search(lonstr, y.standard_name): lon = y
        if search(latstr, x.standard_name): lat = x
        if search(latstr, y.standard_name): lat = y

        lataxis = TransientAxis2D(lat, 
                       axes=(iaxis, jaxis), 
                       attributes={'units':lat.units}, 
                       id=lat.standard_name)
        lonaxis = TransientAxis2D(lon, 
                       axes=(iaxis, jaxis), 
                       attributes={'units':lon.units}, 
                       id=lon.standard_name)

        # Define the combined grid
        grid = TransientCurveGrid(lataxis, lonaxis, id=gridid)
        return grid

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
 
