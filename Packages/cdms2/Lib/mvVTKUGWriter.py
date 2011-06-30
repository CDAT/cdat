#!/usr/bin/env python

"""
Write data to VTK file format using the unstructured grid format
Alex Pletzer, Tech-X Corp. (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""

import numpy
import time
import mvBaseWriter

class VTKUGWriter(mvBaseWriter.BaseWriter):

    def write(self, filename):
        """
        Write file
        @param filename file name
        """
        f = open(filename, 'w')
        print >> f, '# vtk DataFile Version 2.0'
        print >> f, 'generated on %s' % time.asctime()
        print >> f, 'ASCII'
        print >> f, 'DATASET UNSTRUCTURED_GRID'
        npts = self.mesh.shape[0]
        print >> f, 'POINTS %d float' % npts
        for i in range(npts):
            print >> f, '%f %f %f' % tuple(self.mesh[i,:])
        n0, n1, n2 = self.shape
        ncells = (n0 - 1)*(n1 - 1)*(n2 - 1)
        if ncells != 0:
            # 3d
            ntot = ncells * (8 + 1)
            print >> f, 'CELLS %d %d' % (ncells, ntot)
            for k in range(n0 - 1):
                for j in range(n1 - 1):
                    for i in range(n2 - 1):
                        index = i + n2*(j + n1*k)
                        print >> f, '8 %d %d %d %d %d %d %d %d' % \
                            (index, index+1, index+1+n2, index+n2, 
                             index+n1*n2, index+n1*n2+1, 
                             index+n1*n2+1+n2, index+n1*n2+n2)
            print >> f, 'CELL_TYPES %d' % ncells
            for i in range(ncells):
                # hexahedron
                print >> f, 12
            # nodal data
            print >> f, 'POINT_DATA %d' % (n0*n1*n2)
            print >> f, 'SCALARS %s float' % (self.var.id)
            print >> f, 'LOOKUP_TABLE default'
            for k in range(n0):
                for j in range(n1):
                    for i in range(n2):
                        print >> f, '%f' % self.var[k, j, i]
        else:
            # 2d
            ncells = (n1 - 1)*(n2 - 1)
            ntot = ncells * (4 + 1)
            print >> f, 'CELLS %d %d' % (ncells, ntot)
            for j in range(n1 - 1):
                for i in range(n2 - 1):
                    index = i + n2*j
                    print >> f, '4 %d %d %d %d' % \
                        (index, index+1, index+1+n2, index+n2)
            print >> f, 'CELL_TYPES %d' % ncells
            for i in range(ncells):
                # quad
                print >> f, 9
            # nodal data
            print >> f, 'POINT_DATA %d' % (n0*n1*n2)
            print >> f, 'SCALARS %s float' % (self.var.id)
            print >> f, 'LOOKUP_TABLE default'
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
    vw = VTKUGWriter(var)
    vw.write('test2DRect.vtk')

def test3D():
    import cdms2
    var = cdms2.open('sample_data/ta_ncep_87-6-88-4.nc', 'r')('ta')
    vw = VTKUGWriter(var[0,0:10,0:20,0:30])
    vw.write('test3D.vtk')

if __name__ == '__main__': 
    test2DRect()
    test3D()
    
