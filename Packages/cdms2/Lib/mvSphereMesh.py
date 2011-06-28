#!/usr/bin/env python

"""
Class for representing grids on the sphere
Alex Pletzer
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
            self.minElvs = min(elvs[:])
            self.maxElvs = max(elvs[:])
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
        sz = reduce(lambda x,y:x*y, self.shape)
        self.lons = numpy.reshape(self.lons, (sz,))
        self.lats = numpy.reshape(self.lats, (sz,))
        self.elvs = numpy.reshape(self.elvs, (sz,))

    def getXYZCoords(self, sphereRadius=1.0, maxElev=0.1):
        """
        Get the curvilinear cartesian coordinates
        @param sphereRadius radius of sphere 
        @param maxElev maximum elevation 
        """
        sz = reduce(lambda x,y:x*y, self.shape)
        if self.elvPositiveDown:
            rr = sphereRadius*(1.0 - self.elvs)
        else:
            rr = sphereRadius*(1.0 + self.elvs)        
        cosLats = numpy.cos(self.lats*numpy.pi/180.)
        xx = rr*numpy.cos(self.lons*numpy.pi/180.)*cosLats
        yy = rr*numpy.sin(self.lons*numpy.pi/180.)*cosLats
        zz = rr*numpy.sin(self.lats*numpy.pi/180.)
        mesh = numpy.zeros( (sz, 3), numpy.float32 )
        mesh[:, 0] = xx
        mesh[:, 1] = yy
        mesh[:, 2] = zz
        return mesh


#####################################################################
# Tests

def test2DRect():
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
    sm = SphereMesh(var)
    print sm.getXYZCoords()

def test2D():
    import cdms2
    from numpy import pi, cos, sin
    nlat, nlon = 12, 15
    dlon, dlat = 60.0/float(nlon - 1), 30.0/float(nlat - 1)
    lons1D = numpy.array([0.0 + i*dlon for i in range(nlon)])
    lats1D = numpy.array([0.0 + i*dlat for i in range(nlat)])
    lons = numpy.outer(numpy.ones((nlat,)), lons1D)
    lats = numpy.outer(lats1D, numpy.ones((nlon,)))
    data = numpy.outer(cos(3*pi*lats/180.0), 
                       sin(5*pi*lons/180.0))
    var = cdms2.createVariable(data, id='fake_data_2d', 
                               axes=(lats, lons))
    sm = SphereMesh(var)
    print sm.getXYZCoords()

def test3D():
    pass

def test3DRect():
    import cdms2
    from numpy import pi, cos, sin
    nelv, nlat, nlon = 4, 12, 15
    delv, dlon, dlat = 90000./float(nelv-1), 60.0/float(nlon-1), 30.0/float(nlat-1)
    elvs1D = numpy.array([100000 - i*delv for i in range(nelv)])
    lons1D = numpy.array([0.0 + i*dlon for i in range(nlon)])
    lats1D = numpy.array([0.0 + i*dlat for i in range(nlat)])
    # any order should work
    lons = numpy.zeros( (nlon, nlat, nelv), numpy.float32 )
    lats = numpy.zeros( (nlon, nlat, nelv), numpy.float32 )
    elvs = numpy.zeros( (nlon, nlat, nelv), numpy.float32 )
    for i in range(nlon):
        for j in range(nlat):
            for k in range(nelv):
                elvs[i, j, k] = elvs1D[k]
                lats[i, j, k] = lats1D[j]
                lons[i, j, k] = lons1D[i]
    var = cdms2.createVariable(data, id='fake_data_3d_rect', 
                               axes=(elvs, lats, lons))
    sm = SphereMesh(var)
    print sm.getXYZCoords()
    
def test3D():
    import cdms2
    from numpy import pi, cos, sin
    nelv, nlat, nlon = 4, 12, 15
    delv, dlon, dlat = 90000./float(nelv-1), 60.0/float(nlon-1), 30.0/float(nlat-1)
    elvs1D = numpy.array([100000 - i*delv for i in range(nelv)])
    lons1D = numpy.array([0.0 + i*dlon for i in range(nlon)])
    lats1D = numpy.array([0.0 + i*dlat for i in range(nlat)])
    var = cdms2.createVariable(data, id='fake_data_3d_rect', 
                               axes=(elvs1D, lats1D, lons1D))
    sm = SphereMesh(var)
    print sm.getXYZCoords()
    

if __name__ == '__main__': 
    test2DRect()
    test2D()
    test3DRect()
    test3D()
