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
    
    def __init__(self, var, sphereThickness=0.1):
        """
        Constructor
        @param var cdms2 variable
        @param sphereThickness thickness of the shell in normalized 
                               sphere radius
        """
       
        self.isRectilinear = True
        self.ndims = 0
        self.elvPositiveDown = False
        self.minElv = 0
        self.maxElv = 1
        self.sphereThickness = sphereThickness

        # get the lon, lat, elv coordinates (or axes)
        lons = var.getLongitude()
        lats = var.getLatitude()
        elvs = var.getLevel()

        # compute the min/max of elevation, needed
        # for normalization
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
            # apply tensore product of axes to generat curvilinear coordinates
            if type(elvs) != NoneType:
                self.elvs = numpy.outer(numpy.outer( numpy.ones(self.shape[:0], numpy.float32), elvs),
                                        numpy.ones(self.shape[0+1:], numpy.float32)).reshape(self.shape)
            else:
                self.elvs = numpy.zeros( self.shape, numpy.float32 )
            self.lats = numpy.outer(numpy.outer( numpy.ones(self.shape[:1], numpy.float32), lats),
                                    numpy.ones(self.shape[1+1:], numpy.float32)).reshape(self.shape)
            self.lons = numpy.outer(numpy.outer( numpy.ones(self.shape[:2], numpy.float32), lons),
                                    numpy.ones(self.shape[2+1:], numpy.float32)).reshape(self.shape)
    
        else:
            # already in curvilinear form
            self.lons = lons[:]
            self.lats = lats[:]
            if type(elvs) != NoneType:
                self.elvs = elvs[:]
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
        rr = sphereRadius*(1.0 + self.elvs)
        diffElv = self.maxElv - self.minElv
        rr = sphereRadius*numpy.ones(self.lons.shape, numpy.float32 )
        if diffElv != 0:
            coeff = sphereRadius*self.sphereThickness/diffElv
            if self.elvPositiveDown:
                # depth
                rr += coeff*(self.maxElv - self.elvs)
            else:
                # height
                rr += coeff*(self.elvs - self.minElv)

        mesh = numpy.zeros( (sz, 3), numpy.float32 )
        cosLats = numpy.cos( self.lats*numpy.pi/180. )
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
    sphere_mesh = SphereMesh(var, 0.1)
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
    print sphereMesh.getXYZCoords()

def test3DposDown():
    """
    Test 3d data with elev positive down. Need to work with 1D axes.
    """
    print 'test positive down'
    import cdms2
    import numpy
    nlev, nlat, nlon = 4, 5, 6
    dlev, dlat, dlon = 5000./float(nlev-1), 180./float(nlat-1), 360./float(nlon-1)
    levs1d = numpy.arange(0., 5001., dlev)
    lats1d = numpy.array([0. - i*dlat for i in range(nlat)])
    lons1d = numpy.array([0. - i*dlon for i in range(nlon)])
    data = numpy.zeros((nlev, nlat, nlon), numpy.float32)

    for k in range(nlev):
        for j in range(nlat):
            for i in range(nlon):
                data[k, j, i] = numpy.cos(3*numpy.pi*lats1d[j]/180.) * \
                                numpy.sin(5*numpy.pi*lons1d[i]/180.) * \
                                numpy.exp(-levs1d[k])

    a1 = cdms2.axis.TransientAxis(levs1d, id = 'levels', 
                                  attributes = {'positive':'down'})
    a2 = cdms2.axis.TransientAxis(lats1d, id = 'latitude')
    a3 = cdms2.axis.TransientAxis(lons1d, id = 'longitude')
    var = cdms2.createVariable(data, id = 'pos_down_3d_data',
                               axes = (a1, a2, a3))
    sphereMesh = SphereMesh(var)
    aa = sphereMesh.getXYZCoords()
    bb = aa.reshape((4, 5, 6, 3))
    for i in range(nlev): print levs1d[i], bb[i, 0, 0, :]

if __name__ == '__main__': 
#    test2DRect()
#    test2D()
#    test3DRect()
    test3DposDown()
