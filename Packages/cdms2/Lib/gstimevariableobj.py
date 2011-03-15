#/usr/bin/env python

"""
A variable-like object extending over multiple tiles and time slices
$Id: $
"""

import operator
import ctypes
import cdms2
from cdms2.MV2 import concatenate
from cdms2.gsstaticvariableobj import GsStaticVariableObj
from cdms2.error import CDMSError
from cdms2.hgrid import AbstractCurveGrid, TransientCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
import types

import sys

class GsTimeVariableObj(GsStaticVariableObj):

    def __init__(self, GsHost, varName, **slicekwargs):
        """
        Constructor
        @param GsHost host object 
        @param varName variable name
        @param slicekwargs eg lon=(-180,180), lat=(-90,90), time=5
                           cf Packages/cdms2/Lib/cudsinterface.py for 
                           a list of keywords
        """
        from re import search
        self.varName = varName
        self.ntimeSlices = GsHost.ntimeSlices

        self.vars = []
        if self.ntimeSlices > 0:
            self.vars = [None for i in range(GsHost.ngrids)]

        kwargs = {}
        for k in slicekwargs.keys():
            kwargs[k.lower()] = slicekwargs[k]

        # time dependent variable. Concatenate the current file variable
        # into the last for a given grid. This builds a consistent
        # variable across time.
        if ('time' in kwargs.keys() and len(slicekwargs) <= 1) or \
                len(slicekwargs) == 0:
            for gfindx in range(GsHost.ngrids):

                # Create the horizontal curvilinear grid.
                # But how do I add the time grid? I don't know it yet.
                # It is known after looping over the time files for a given
                # variable
                gFName = GsHost.gridFilenames[gfindx]
                
                for tfindx in range(GsHost.ntimeSlices):
                    fName = GsHost.timeDepVars[varName][tfindx][gfindx]
                    fh = cdms2.open(fName)
                    # TransientVariable
                    try:
                        var = fh(varName, **slicekwargs)
                    except:
                        continue

                    # Attach the grid to the variable
                    grid = self.createGrid(gFName, var.attributes['coordinates'])
                    axis0 = var.getAxis(0)
                    gridaxes = grid.getAxisList()
                    axes = [axis0, gridaxes[0], gridaxes[1]]

                    # Create cdms2 transient variable
                    if tfindx == 0:
                        new = cdms2.createVariable(var, 
                                    axes = axes, 
                                    grid = grid, 
                                    attributes = var.attributes, 
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
                                    attributes = var.attributes, 
                                    id = var.standard_name)
                        
                    fh.close()

                # Add the variable to the index
                self.vars[gfindx]              = new

                # Add some other attributes
                self.vars[gfindx].tile_name    = fh.tile_name
                self.vars[gfindx].gridFilename = gFName

    def __getitem__(self, tfindx):
        """
        Data accessor
        @param tfindx file file index
        @return variable at tfindx
        """
        return self.vars[tfindx]

    def __setitem__(self, tfindx, vals):

        """
        Data setter
        @param tfindx time file indexer
        @param vals values to set
        """
        self.vars[tfindx] = vals

    def shape(self, gfindx):
        """
        Return the shape in the format (n0, n1, ...)
        @param gfindx time file index
        @return result
        """
        v = self.vars[gfindx]
        if v:
            return self.vars[tfindx].shape
        else:
            return []

    def size(self):
        """
        Return the total number of elements
        @return number of elements
        """
        return reduce(operator.mul, [v.size() for v in self.vars])

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
            res += (" Grid slice %d: " % gfindx) + repr(self.vars[gfindx])
        return res

###################################################################
def test():
    pass

if __name__ == '__main__': test()

