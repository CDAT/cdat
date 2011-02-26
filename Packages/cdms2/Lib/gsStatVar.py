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
from cdms2.hgrid import AbstractCurveGrid

class GsStatVar:
    """
    Open a static variable.
    """

    def __init__(self, GsHost, varName):
        """
        Constructor
        @param varName variable name
        @param ngrids number of grids
        """
        self.varName = varName
        self.ngrids = GsHost.ngrids

        self.vars = []
        if self.ngrids > 0:
            self.vars = [None for i in range(self.ngrids)]

        for gfindx in range(self.ngrids):
            fName = GsHost.statVars[varName][gfindx]
            gName = GsHost.gridFilenames[gfindx]
            fh = cdms2.open(fName)

            gh = cdms2.open(gName)

            vr = fh(varName)
            vr.gridFilename = gName
            vr.gridIndex    = gfindx

            # Add some methods to GsStatVar[gfindx]
            updateGetLatitudeToAbstractVariable(vr)
            updateGetLongitudeToAbstractVariable(vr)
            updateSetGridToAbstractVariable(vr)
            updateGetGridToAbstractVariable(vr)
            addGetCoordinatesToAbstractVariable(vr)

            vr.setGrid()
            self.vars[gfindx] = vr

            fh.close()

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
        print dir(self)
#        for gfindx in range(len(self.vars)):
#            res += (" grid %d: " % gfindx) + repr(self.vars[gfindx])
#        return res

###############################################################################

# Add getCoordinates and use these versions of getLongitude and getLatitude
# for each grid in a GsStatVar
#
# This is not very satisfactory, since I am having to duplicate the code in
# gsTimeVar.py
def updateGetLatitudeToAbstractVariable(AbstractVariable):
    """
    Update the AbstractVariable method getLatitude
    @param AbstractVariable a Abstract variable
    """
    def getLatitude(self):
        """
        Return the coordinate data associated with variable
        @return latitude from a cdms2.hgrid.AbstrctCurveGrid
        """
        if 'coordinates' in self.attributes.keys():
          return self.getGrid().getLatitude()

        else:
            raise CDMSError, "No 'coordinates' attribute. Can't getLatitude"
            

    # Add getLatitude to the AbstractVariable Class
    AbstractVariable.getLatitude = types.MethodType(getLatitude, AbstractVariable)

def updateGetLongitudeToAbstractVariable(AbstractVariable):
    """
    Update the AbstractVariable method getLongitude
    @param AbstractVariable a Abstract variable
    """
    def getLongitude(self):
        """
        Return the coordinate data associated with variable
        @return longitude from a cdms2.hgrid.AbstrctCurveGrid
        """

        if 'coordinates' in self.attributes.keys():
          return self.getGrid().getLongitude()
        else:
            raise CDMSError, "No 'coordinates' attribute. Can't getLongitude"

    # Add getLongitude to the AbstractVariable Class
    AbstractVariable.getLongitude = types.MethodType(getLongitude, AbstractVariable)

def updateSetGridToAbstractVariable(AbstractVariable):
    """
    Update setGrid in the Class AbstractVariable
    @param AbstractVariable a Abstract variable
    """
    def setGrid(self):
        """
        Return the coordinate data associated with variable
        @return grid a cdms2.hgrid.AbstractCurveGrid object
        """

        fh = cdms2.open(self.gridFilename)
        if 'coordinates' in self.attributes.keys():
            xn, yn = self.attributes['coordinates'].split()
    
            x = fh(xn)
            y = fh(yn)
            self.grid = AbstractCurveGrid(x, y)
            self._lonaxis_ = self.grid._lonaxis_
            self._lataxis_ = self.grid._lataxis_
            if self.rank() != len(self.grid.getAxisList()):
                raise CDMSError, """self.rank doesn't match the number of axes 
                                    for the grid"""
            for i in range(self.rank()):
                self.setAxis(i, self._lonaxis_.getAxis(i))
                self.setAxis(i, self._lataxis_.getAxis(i))

        else:
            raise CDMSError, "No 'coordinates' attribute. Can't getLongitude"

    # Add getGrids to the AbstractVariable Class
    AbstractVariable.setGrid = types.MethodType(setGrid, AbstractVariable)

def updateGetGridToAbstractVariable(AbstractVariable):
    """
    Update getGrid in the Class AbstractVariable
    @param AbstractVariable a Abstract variable
    """
    def getGrid(self):
        """
        Return the coordinate data associated with variable
        @return grid a cdms2.hgrid.AbstrctCurveGrid object
        """

        return self.grid

    # Add getGrids to the AbstractVariable Class
    AbstractVariable.getGrid = types.MethodType(getGrid, AbstractVariable)

def addGetCoordinatesToAbstractVariable(AbstractVariable):
    """
    Add getCoordinates to the Class AbstractVariable
    @param AbstractVariable a Abstract variable
    """
    def getCoordinates(self):
        """
        Return the coordinate data associated with variable
        @return tuple of Grids longitude and latitude
        """

        fh = cdms2.open(self.gridFilename)
        if 'coordinates' in self.attributes.keys():
            xn, yn = self.attributes['coordinates'].split()
    
            x = fh(xn)
            y = fh(yn)
    
            return (x, y)
        else:
            raise CDMSError, "No 'coordinates' attribute. Can't getLongitude"

    # Add getCoordinates to the AbstractVariable Class
    AbstractVariable.getCoordinates = types.MethodType(getCoordinates, AbstractVariable)

###################################################################
def test():
    pass

if __name__ == '__main__': test()
 
