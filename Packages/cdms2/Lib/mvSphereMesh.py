#!/usr/bin/env python

"""
Class for representing grids on the sphere
Alex Pletzer, Tech-X Corp. (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""

import numpy
from types import NoneType

class SphereMesh:
    
    def __init__(self, var):
        """
        Constructor
        @param var cdms2 variable
        """
       
        self.isRectilinear = True
        self.ndims = 0
        self.elvPositiveDown = False
        self.minElv = 0
        self.maxElv = 1

        # get the lon, lat, elv coordinates (or axes)
        lons = var.getLongitude()
        lats = var.getLatitude()
        elvs = var.getLevel()

        # compute the min/max of elevation, we will
        # normalize
        if type(elvs) != NoneType:
            self.minElv = min(elvs[:])
            self.maxElv = max(elvs[:])
            if hasattr(elvs, 'positive'):
                if getattr(elvs, 'positive') == 'down':
                    self.elvPositiveDown = True

        # determine the dimensionality and 
        # whether the grid is rectilinear
        for axis in lons, lats, elvs:
            if type(axis) != NoneType:
                self.ndims += 1
                if len(axis.shape) != 1:
                    self.isRectilinear = False

        self.shape = lons.shape
        if self.isRectilinear:
            self.shape = []
            for axis in lons, lats, elvs:
                if type(axis) != NoneType:
                    self.shape.append( len(axis) )
            self.shape.reverse()

        while len(self.shape) < 3:
            self.shape = [1,] + list(self.shape)

        # store lon, lat, elv as a curvilinear grid
        if self.isRectilinear:
            self.lons = numpy.zeros( self.shape, numpy.float32 )
            self.lats = numpy.zeros( self.shape, numpy.float32 )
            self.elvs = numpy.zeros( self.shape, numpy.float32 )
            for k in range(self.shape[0]):
                for j in range(self.shape[1]):
                    for i in range(self.shape[2]):
                        self.lons[k, j, i] = lons[i]
                        self.lats[k, j, i] = lats[j]
                        if type(elvs) != NoneType:
                            self.elvs[k, j, i] = (elvs[k] - self.minElv) / \
                                (self.maxElv - self.minElv)
                        else:
                            self.elvs[k, j, i] = 0
        else:
            # already in curvilinear form
            self.lons = lons[:]
            self.lats = lats[:]
            if type(elvs) != NoneType:
                self.elvs = (elvs - self.minElv)(self.maxElv - self.minElv)
            else:
                self.elvs = numpy.zeros( self.shape, numpy.float32 )

        # reshape as flat arrays
        sz = reduce(lambda x, y: x*y, self.shape)
        self.lons = numpy.reshape(self.lons, (sz,))
        self.lats = numpy.reshape(self.lats, (sz,))
        self.elvs = numpy.reshape(self.elvs, (sz,))

    def getXYZCoords(self, sphereRadius=1.0):
        """
        Get the curvilinear cartesian coordinates
        @param sphereRadius radius of sphere 
        @return mesh
        """
        sz = reduce(lambda x, y: x*y, self.shape)
        elvMax = max(self.elvs[:])
        elvMin = min(self.elvs[:])
        diffElv = elvMax - elvMin
        rr = sphereRadius*numpy.ones(self.lons.shape, numpy.float32 )
        if diffElv > 0:
            if self.elvPositiveDown:
                # depth
                rr = sphereRadius*(1.0 - (self.elvs - elvMax)/diffElv)
            else:
                # height
                rr = sphereRadius*(1.0 + (self.elvs - elvMin)/diffElv)  

        mesh = numpy.zeros( (sz, 3), numpy.float32 )
        cosLats = numpy.cos(self.lats*numpy.pi/180.)
        mesh[:, 0] = rr*numpy.cos(self.lons*numpy.pi/180.)*cosLats
        mesh[:, 1] = rr*numpy.sin(self.lons*numpy.pi/180.)*cosLats
        mesh[:, 2] = rr*numpy.sin(self.lats*numpy.pi/180.)
        return mesh

#####################################################################
# Tests

def test2DRect():
    """
    Test data on 2D rectilinear grid
    """
    import cdms2
    from numpy import pi, cos, sin
    nlat, nlon = 12, 15
    grid = cdms2.createUniformGrid(-0.0, nlat, 60./(nlat-1), 
                                    0., nlon, 30./nlon)
    lons = grid.getLongitude()
    lats = grid.getLatitude()
    data = numpy.outer(cos(3*pi*lats[:]/180.0), 
                       sin(5*pi*lons[:]/180.0))
    var = cdms2.createVariable(data, id='fake_data_2d_rect', 
                               axes=(lats, lons))
    sphere_mesh = SphereMesh(var)
    print sphere_mesh.getXYZCoords()

def test2D():
    """
    Test data on 2D curvilinear grid
    """
    import cdms2
    from cdms2.coord import TransientAxis2D, TransientVirtualAxis
    from cdms2.hgrid import TransientCurveGrid
    from numpy import pi, cos, sin
    nlat, nlon = 3, 4
    dlon, dlat = 60.0/float(nlon - 1), 30.0/float(nlat - 1)
    lons1D = numpy.array([0.0 + i*dlon for i in range(nlon)])
    lats1D = numpy.array([0.0 + j*dlat for j in range(nlat)])
    lons = numpy.outer(numpy.ones((nlat,)), lons1D)
    lats = numpy.outer(lats1D, numpy.ones((nlon,)))
    data = cos(3*pi*lats/180.0) * sin(5*pi*lons/180.0)
    # create grid
    iaxis = TransientVirtualAxis("i", nlon)
    jaxis = TransientVirtualAxis("j", nlat)
    lataxis = TransientAxis2D(lats, 
                       axes=(jaxis, iaxis), 
                       attributes={'units': 'degree_north'}, 
                       id='lats')
    lonaxis = TransientAxis2D(lons, 
                       axes=(jaxis, iaxis), 
                       attributes={'units': 'degree_east'}, 
                       id='lons')
    grid =  TransientCurveGrid(lataxis, lonaxis, id='lats_lons')

    var = cdms2.createVariable(data, id='fake_data_2d', 
                               axes = grid.getAxisList(),
                               grid = grid,
                               attributes = {'coordinates': 'lats lons'},
                               )
    sphere_mesh = SphereMesh(var)
    print sphere_mesh.getXYZCoords()

def test3DRect():
    """
    Test data on 3d rectilinear grid
    """
    import cdms2
    from numpy import pi, cos, sin, exp
    nelv, nlat, nlon = 3, 4, 5
    delv, dlon, dlat = 90000./float(nelv-1), \
        60.0/float(nlon-1), 30.0/float(nlat-1)
    elvs1D = numpy.array([100000 - i*delv for i in range(nelv)])
    lons1D = numpy.array([0.0 + i*dlon for i in range(nlon)])
    lats1D = numpy.array([0.0 + i*dlat for i in range(nlat)])
    # any order should work
    lons = numpy.zeros( (nlon, nlat, nelv), numpy.float32 )
    lats = numpy.zeros( (nlon, nlat, nelv), numpy.float32 )
    elvs = numpy.zeros( (nlon, nlat, nelv), numpy.float32 )
    data = numpy.zeros( (nlon, nlat, nelv), numpy.float32 )
    for i in range(nlon):
        for j in range(nlat):
            for k in range(nelv):
                elvs[i, j, k] = elvs1D[k]
                lats[i, j, k] = lats1D[j]
                lons[i, j, k] = lons1D[i]
                data[i, j, k] = cos(3*pi*lats[i, j, k]/180.) * \
                    sin(5*pi*lons[i, j, k]/180.) * exp(-elvs[i, j, k])
    var = cdms2.createVariable(data, id='fake_data_3d_rect', 
                               axes=(elvs, lats, lons))
    sphere_mesh = SphereMesh(var)
    print sphere_mesh.getXYZCoords()
        

if __name__ == '__main__': 
    test2DRect()
    test2D()
    test3DRect()
