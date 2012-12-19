"""
Test mvGenericRegrid class
$Id: testMvCdmsRegrid.py 2354 2012-07-11 15:28:14Z pletzer $
"""

import cdms2
import numpy
import unittest
import ESMP
import matplotlib.pylab as pl
from mpi4py import MPI
from cdms2 import CdmsRegrid
import sys

class Test(unittest.TestCase):
    """
    All test interpolate to the same grid
    """

    def setUp(self):
        """
        Set up the grids to pass to mvGenericRegrid
        """
        self.comm = MPI.COMM_WORLD
        self.rank = self.comm.Get_rank()
        self.size = self.comm.Get_size()

        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        self.so = f('so')

        g = cdms2.open(sys.prefix + '/sample_data/clt.nc')
        self.clt = g('clt')

    def testSingleTimeSingleElev(self):
        """
        Interpolate over one level/time
        """
    
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc')
        clt = f('clt')
        v = f('v')[0,0,...]
        
        srcGrid = v.getGrid()
        dstGrid = clt.getGrid()
        ro = CdmsRegrid(srcGrid = srcGrid, 
                        dstGrid = dstGrid,
                        dtype = v.dtype)

        vInterp = ro(v)

        print 'min/max of v: %f %f' % (v.min(), v.max())
        print 'min/max of vInterp: %f %f' % (vInterp.min(), vInterp.max())

        if False:
            pl.figure()
            pl.pcolor(vInterp, vmin=-20, vmax=20)
            pl.title('testSingleTimeSingleElev: vInterp')
            pl.colorbar()

    def testMultipleTimesAndElevations(self):
        """
        Interpolate over time and elevation axes
        """
    
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc')
        clt = f('clt')
        v = f('v')
        
        srcGrid = v.getGrid()
        dstGrid = clt.getGrid()
        ro = CdmsRegrid(srcGrid = srcGrid, 
                        dstGrid = dstGrid, 
                        dtype = v.dtype)

        vInterp = ro(v)

        mask = (v == v.missing_value)
        
        if self.rank == 0:
            vMin, vMax = v.min(), (v*(1-mask)).max()
            vInterpMin, vInterpMax = vInterp.min(), (vInterp).max()
            print 'min/max of v: %f %f' % (vMin, vMax)
            print 'min/max of vInterp: %f %f' % (vInterpMin, vInterpMax)
            self.assertLess(abs(vMin - vInterpMin), 0.4)
            self.assertLess(abs(vMax - vInterpMax), 0.2)

        if False and self.rank == 0:
            nTimes = v.shape[0]
            nLevels = v.shape[1]
            for el in range(nTimes):
                for k in range(nLevels):
                    pl.figure()
                    pl.subplot(1,2,1)
                    pl.pcolor(v[el, k,...], vmin=-20, vmax=20)
                    pl.title('testMultipleTimesAndElevations: v[%d, %d,...]' % (el, k))
                    pl.colorbar()
                    pl.subplot(1,2,2)
                    pl.pcolor(vInterp[el, k,...], vmin=-20, vmax=20)
                    pl.title('testMultipleTimesAndElevations: vInterp[%d, %d,...]' % (el, k))
                    pl.colorbar()
    
    def testSalinityModel(self):
        print "\nACCESS Salinity model"
        srcGrid = self.so.getGrid()
        dstGrid = self.clt.getGrid()

        ro = CdmsRegrid(srcGrid = srcGrid, 
                        dstGrid = dstGrid,
                        dtype = self.so.dtype,
                        regridTool = 'gsregrid', # "ESMp",
                        regridMethod = "Linear")

        soInterp = ro(self.so)
        print 'type(soInterp) = ', type(soInterp)
        soMin, soMax = self.so.min(), self.so.max()
        print "min/max of self.so: %f %f" % (soMin, soMax)
        soInterpMin, soInterpMax = soInterp.min(), soInterp.max()
        print "min/max of soInterp: %f %f" % (soInterpMin, soInterpMax)
        self.assertEqual(self.so.missing_value, soInterp.missing_value)
        self.assertLess(soInterpMax, 1.01*soMax)
        self.assertLess(0.99*soMin, soInterpMin)
        self.assertEqual(soInterp.shape[0], self.so.shape[0])
        self.assertEqual(soInterp.shape[1], self.so.shape[1])
        self.assertEqual(soInterp.shape[2], dstGrid.shape[0])
        self.assertEqual(soInterp.shape[3], dstGrid.shape[1])

        if False:
            nTimes = self.so.shape[0]
            nLevels = 3
            for time in range(nTimes):
                pl.figure(time)
                f = 1
                for k in [0,15,30]:
                    pl.subplot(3,2,f)
                    pl.pcolor(self.so[time, k, ...], vmin = 20, vmax = 40)
                    pl.title("so[%d, %d,...]" % (time, k))
                    pl.colorbar()
                    pl.subplot(3,2,f+1)
                    pl.pcolor(soInterp[time, k, ...], vmin = 20, vmax = 40)
                    pl.title("so[%d, %d,...]" % (time, k))
                    pl.colorbar()
                    f+=2
                pl.suptitle("ACCESS Salinity Test for Time + Levels")


if __name__ == '__main__':
    print "" # Spacer
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pl.show()


