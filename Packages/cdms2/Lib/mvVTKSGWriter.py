#!/usr/bin/env python

"""
Write data to VTK file format using the structured grid format
Alex Pletzer, Tech-X Corp. (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""

import numpy
import time
import mvBaseWriter

class VTKSGWriter(mvBaseWriter.BaseWriter):

    def write(self, filename):
        """
        Write file
        @param filename file name
        """
        f = open(filename, 'w')
        print >> f, '# vtk DataFile Version 2.0'
        print >> f, 'generated on %s' % time.asctime()
        print >> f, 'ASCII'
        print >> f, 'DATASET STRUCTURED_GRID'
        shp = self.shape[:]
        shp.reverse()
        print >> f, 'DIMENSIONS %d %d %d' % tuple(shp)
        npts = self.mesh.shape[0]
        print >> f, 'POINTS %d float' % npts
        for i in range(npts):
            print >> f, '%f %f %f' % tuple(self.mesh[i,:])
        n0, n1, n2 = self.shape
        # nodal data
        print >> f, 'POINT_DATA %d' % (n0*n1*n2)
        print >> f, 'SCALARS %s float' % (self.var.id)
        print >> f, 'LOOKUP_TABLE default'
        if n0 > 1:
            for k in range(n0):
                for j in range(n1):
                    for i in range(n2):
                        print >> f, '%f' % self.var[k, j, i]
        else:
            for j in range(n1):
                for i in range(n2):
                    print >> f, '%f' % self.var[j, i]            
        f.close()


######################################################################

def test2DRect():
    import cdms2
    from numpy import pi, cos, sin
    nlat, nlon = 6, 10
    grid = cdms2.createUniformGrid(-0.0, nlat, 60./(nlat-1), 
                                    0., nlon, 30./nlon)
    lons = grid.getLongitude()
    lats = grid.getLatitude()
    data = numpy.outer(cos(3*pi*lats[:]/180.0), 
                       sin(5*pi*lons[:]/180.0))
    var = cdms2.createVariable(data, id='fake_data_2d_rect', 
                               axes=(lats, lons))
    vw = VTKSGWriter(var)
    vw.write('test2DRect_SG.vtk')

def test3D():
    import cdms2
    var = cdms2.open('sample_data/ta_ncep_87-6-88-4.nc', 'r')('ta')
    vw = VTKSGWriter(var[0,0:10,0:20,0:30])
    vw.write('test3D_SG.vtk')

if __name__ == '__main__': 
    test2DRect()
    test3D()
    
