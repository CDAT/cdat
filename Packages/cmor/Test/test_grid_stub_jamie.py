'''Tests around pywrapper - need an environment to pick up the stub _cmor implementation'''
import numpy

import unittest
from cmor import grid

import _cmor_stub #this should be a stub cmor implementation

class TestGrid(unittest.TestCase):
    
    def test_pass_only_axis_ids(self):
        for axis_ids in ([1,2], numpy.array([1,2])):
            args = grid(axis_ids)
            self.assert_on_axis_id_args(axis_ids, args)
            self.assert_on_default_coordinates(args)
            self.assert_on_default_vertices(args)
            
    def test_lat_lon_grid(self):
        axis_ids = [1, 2]
        for atype in (numpy.array, list, tuple):
            lats = atype(numpy.arange(2*3).reshape(2,3) + 0.1)
            lons = atype(numpy.arange(2*3).reshape(2,3) - 0.1)
            args = grid(axis_ids, latitude = lats, longitude = lons)
            self.assert_on_axis_id_args(axis_ids, args)
            self.assert_on_coordiantes(lats, lons, args)
            self.assert_on_default_vertices(args)

    def test_lat_lon_with_vertices(self):
        axis_ids = [1, 2]
        nvert = 4
        for atype in (numpy.array, list, tuple):
            lats = atype(numpy.arange(2*3).reshape(2,3) + 0.1)
            lons = atype(numpy.arange(2*3).reshape(2,3) - 0.1)
            lat_vert = atype(numpy.arange(2*3*nvert).reshape(2,3,nvert) + 0.5)
            lon_vert = atype(numpy.arange(2*3*nvert).reshape(2,3,nvert) - 0.5)
            args = grid(axis_ids,
                        latitude = lats,
                        longitude = lons,
                        latitude_vertices = lat_vert,
                        longitude_vertices = lon_vert)
            self.assert_on_axis_id_args(axis_ids, args)
            self.assert_on_coordiantes(lats, lons, args)
            self.assert_on_vertices(nvert, lat_vert, lon_vert, args)

    def assert_on_axis_id_args(self, axis_ids, args):
        self.assertEquals(len(axis_ids), args[0])
        self.assertTrue(((axis_ids) == args[1]).all())

    def assert_on_coordiantes(self, lats, lons, args):
        self.assertEquals('d', args[2])
        self.assertTrue((lats == args[3]).all())
        self.assertTrue((lons == args[4]).all())

    def assert_on_vertices(self, nvert, lat_vert, lon_vert, args):
        self.assertEquals(nvert, args[5])
        self.assertTrue((lat_vert == args[6]).all())
        self.assertTrue((lon_vert == args[7]).all())
        
    def assert_on_default_coordinates(self, args):
        self.assertEquals('f', args[2])
        self.assertEquals(None, args[3])
        self.assertEquals(None, args[4])
        
    def assert_on_default_vertices(self, args):
        self.assertEquals(0, args[5])
        self.assertEquals(None, args[6])
        self.assertEquals(None, args[7])

class TestGridCallErrors(unittest.TestCase):
    def test_error_rank_axis_ids(self):
        try:
            grid([[1], [2]])
            self.fail('should raise exception')
        except Exception, e:
            self.assertEquals('error axes list/array must be 1D', str(e))
            
    def test_error_on_axis_ids(self):
        bad_axis_ids = ( 0, 'astring')
        for axis_ids in bad_axis_ids:
            try:
                grid(axis_ids)
                self.fail('should raise exception')
            except Exception, e:
                self.assertEquals('Error could not convert axis_ids list to a numpy array',
                                  str(e))

    def test_error_latitude_no_longitude(self):
        try:
            grid([0], latitude = numpy.arange(2))
            self.fail('should raise exception')
        except Exception, e:
            self.assertEquals("Error could not convert longitude to a numpy array", str(e))

    def test_error_longitude_no_latitude(self):
        try:
            grid([0], longitude = numpy.arange(2))
            self.fail('should raise exception')
        except Exception, e:
            self.assertEquals("latitude and longitude must be BOTH an array or None", str(e))

    def test_error_type_lats(self):
        lons = numpy.arange(2*3).reshape(2,3)
        for lats in (0, 0.1, 'string', {}):
            try:
                grid([0, 11], latitude = lats, longitude = lons)
                self.fail('should raise exception')
            except Exception, e:
                self.assertEquals('Error could not convert latitude to a numpy array', str(e))
    def test_error_type_lons(self):
        lats = numpy.arange(2*3).reshape(2,3)
        for lons in (0, 0.1, 'string', {}):
            try:
                grid([0, 1], latitude = lats, longitude = lons)
                self.fail('should raise exception')
            except Exception, e:
                self.assertEquals('Error could not convert longitude to a numpy array', str(e))            
    def test_error_rank_lons(self):
        axis_ids = [1, 2]
        lats = numpy.arange(2*3).reshape(2,3)
        lons = numpy.arange(3)
        try:
            grid(axis_ids, latitude = lats, longitude = lons)
            self.fail('should raise exception')
        except Exception, e:
            self.assertEquals("longitude's rank does not match number of axes passed via axis_ids", str(e))

    def test_error_rank_lats(self):
        axis_ids = [1, 2]
        lats = numpy.arange(2)
        lons = numpy.arange(2*3).reshape(2,3)
        try:
            grid(axis_ids, latitude = lats, longitude = lons)
            self.fail('should raise exception')
        except Exception, e:
            self.assertEquals("latitude's rank does not match number of axes passed via axis_ids", str(e))

    def test_error_rank_lat_verts(self): # this test may be fragile?
        axis_ids = [1, 2]
        lats = numpy.arange(2*3).reshape(2,3)
        lons = numpy.arange(2*3).reshape(2,3)
        lat_verts = lats
        try:
            grid(axis_ids, latitude = lats, longitude = lons,
                 latitude_vertices = lat_verts)
            self.fail('should raise exception')
        except Exception, e:
            self.assertEquals("latitude_vertices's rank does not match number of axes passed via axis_ids +1 (for vertices)",
            str(e))

    def test_error_rank_lon_verts(self): # this test may be fragile?
        axis_ids = [1, 2]
        lats = numpy.arange(2*3).reshape(2,3)
        lons = numpy.arange(2*3).reshape(2,3)
        lon_verts = lons
        try:
            grid(axis_ids, latitude = lats, longitude = lons,
                 longitude_vertices = lon_verts)
            self.fail('should raise exception')
        except Exception, e:
            self.assertEquals("longitude_vertices's rank does not match number of axes passed via axis_ids +1 (for vertices)",
            str(e))
            
if __name__ == '__main__':
    unittest.main()
