"""
Test from Charles Doutriaux, compares the accuracy of all the
interpolation methods
"""
import unittest
import cdms2
import numpy
import sys

PLOT = False
if PLOT:
    from matplotlib import pylab

class Test(unittest.TestCase):

    def setUp(self):
            pass

    def test0(self):
        """
        One way interpolation
        """
        fnm = sys.prefix + '/sample_data/clt.nc'
        f = cdms2.open(fnm)

        s=f("clt")
        grdori = s.getGrid()
        nLon = s.shape[-1]
        nLat = s.shape[-2]
        xx = numpy.outer(numpy.ones( (nLat,), numpy.float32 ), s.getLongitude()[:])
        yy = numpy.outer(s.getLatitude(), numpy.ones( (nLon,), numpy.float32) )
        print s.shape, xx.shape, yy.shape
        #s[0,...] = 50.0*(1.0 + numpy.sin(4*numpy.pi * xx / 180.0) * numpy.cos(2*numpy.pi * yy / 180.0))

        grid = cdms2.createGaussianGrid(32, 64)
        sInterps = {}
        sInterps['regrid2'] = s.regrid(grid, regridTool='regrid2')
        sInterps['esmf linear'] = s.regrid(grid, regridTool='esmf', regridMethod = 'linear')
        diag = {}
        sInterps['libcf'] = s.regrid(grid, regridTool='libcf', diag=diag)
        print diag
        diag = {}
        sInterps['esmf conserve']  = s.regrid(grid, regridTool='esmf', regridMethod = 'conserve', diag=diag)
        print diag

        diff = abs(sInterps['esmf linear'] - sInterps['regrid2']).max()
        self.assertLess(diff, 18.0)
        diff = abs(sInterps['esmf conserve'] - sInterps['regrid2']).max()
        self.assertLess(diff, 86.0)
        diff = abs(sInterps['libcf'] - sInterps['regrid2']).max()
        self.assertLess(diff, 18.0)

        if PLOT:
            row = 0
            for mth in sInterps:
                row += 1
                pylab.subplot(2, 2, row)
                pylab.pcolor(sInterps[mth][0,...] - sInterps['regrid2'][0,...], vmin = -10, vmax = 10)
                pylab.colorbar()
                pylab.title(mth + ' - regrid2')


    def Xtest1(self):
        """
        Forward/backward interpolation
        """

        fnm=sys.prefix+'/sample_data/clt.nc'
        f=cdms2.open(fnm)

        s=f("clt")
        grdori = s.getGrid()
        nLon = s.shape[-1]
        nLat = s.shape[-2]
        xx = numpy.outer(numpy.ones( (nLat,), numpy.float32 ), s.getLongitude()[:])
        yy = numpy.outer(s.getLatitude(), numpy.ones( (nLon,), numpy.float32) )
        print s.shape, xx.shape, yy.shape
        #s[0,...] = 50.0*(1.0 + numpy.sin(4*numpy.pi * xx / 180.0) * numpy.cos(2*numpy.pi * yy / 180.0))

        grid = cdms2.createGaussianGrid(64,128)
        sInterps = {}
        sInterps['regrid2'] = s.regrid(grid, regridTool='regrid2').regrid(grdori, regridTool='regrid2')
        sInterps['esmf linear'] = s.regrid(grid, regridTool='esmf', regridMethod = 'linear').regrid(grdori, regridTool='esmf', regridMethod = 'linear')
        diag = {}
        sInterps['libcf'] = s.regrid(grid, regridTool='libcf', diag=diag).regrid(grdori, regridTool='libcf', diag=diag)
        print diag
        diag = {}
        sInterps['esmf conserve']  = s.regrid(grid, regridTool='esmf', regridMethod = 'conserve', diag=diag).regrid(grdori, regridTool='esmf', regridMethod = 'conserve')
        print diag

        diff = abs(sInterps['regrid2'] - s).max()
        self.assertLess(diff, 63.0)
        diff = abs(sInterps['esmf linear'] - s).max()
        self.assertLess(diff, 45.0)
        diff = abs(sInterps['esmf conserve'] - s).max()
        self.assertLess(diff, 103.534)
        diff = abs(sInterps['libcf'] - s).max()
        self.assertLess(diff, 45.0)

        if PLOT:
            row = 0
            for mth in sInterps:
                row += 1
                pylab.subplot(2, 2, row)
                pylab.pcolor(sInterps[mth][0,...] - s[0,...], vmin = -10, vmax = 10)
                pylab.colorbar()
                pylab.title(mth + ' - original')

if __name__ == '__main__':
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    a= unittest.TextTestRunner(verbosity = 1).run(suite)
    if PLOT:
        pylab.show()
    sys.exit(len(a.errors))
