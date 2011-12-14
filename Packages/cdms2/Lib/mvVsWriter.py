#!/usr/bin/env python

"""
Write data to VizSchema compliant file
Alex Pletzer, Tech-X Corp. (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""

import numpy
import mvBaseWriter

class VsWriter(mvBaseWriter.BaseWriter):

    def write(self, filename):
        """
        Write file
        @param filename file name
        """
        try:
            import tables
        except:
            raise ImportError, 'You must have pytables installed'
        
        if filename.find('.vsh5') < 0 and filename.find('.h5') < 0:
            filename += '.vsh5' # VizSchema hdf5 format

        # open file
        h5file = tables.openFile(filename, 'w')
        # put mesh
        meshid = 'mesh_' + self.var.id
        mdata = numpy.reshape(self.mesh, self.shape + [3,])
        mset = h5file.createArray("/", meshid, mdata)
        mset.attrs.vsType = "mesh"
        mset.attrs.vsKind = "structured"
        mset.attrs.vsIndexOrder = "compMinorC"
        # data
        dset = h5file.createArray("/", self.var.id, 
                                  numpy.reshape(self.var, self.shape))
        dset.attrs.vsType = "variable"
        dset.attrs.vsMesh = meshid
        # additional attributes
        for a in self.var.attributes:
            setattr(dset.attrs, a, getattr(self.var, a))
        # close file
        h5file.close()

######################################################################

def test2DRect():
    import cdms2
    from numpy import pi, cos, sin
    nlat, nlon = 3, 4
    grid = cdms2.createUniformGrid(-0.0, nlat, 60./(nlat-1), 
                                    0., nlon, 30./nlon)
    lons = grid.getLongitude()
    lats = grid.getLatitude()
    data = numpy.outer(cos(3*pi*lats[:]/180.0), 
                       sin(5*pi*lons[:]/180.0))
    var = cdms2.createVariable(data, id='fake_data_2d_rect', 
                               axes=(lats, lons))
    vw = VsWriter(var)
    vw.write('test2DRect.vsh5')

def test3D():
    import cdms2
    var = cdms2.open('sample_data/ta_ncep_87-6-88-4.nc', 'r')('ta')
    vw = VsWriter(var[0,0:10,0:20,0:30])
    vw.write('test3D.vsh5')

if __name__ == '__main__': 
    test2DRect()
    test3D()
    
