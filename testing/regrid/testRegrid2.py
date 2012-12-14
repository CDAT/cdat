"""
$Id: testRegrid2.py 2324 2012-07-02 19:04:05Z dkindig $

Unit tests for regrid2.gsRegrid

"""

import cdms2
import regrid2
import matplotlib as mpl
import numpy as np
import unittest
import openCreateData
from matplotlib import pylab

class TestTimeRegridding(unittest.TestCase):

    def setUp(self):
        
        filename = "so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc"
        h=cdms2.open('./' + filename)
        self.hso = h('so')[0, 0, ...]
        self.hGrid = [self.hso.getLatitude(), self.hso.getLongitude()]
        self.grid2D = self.hso.getGrid()

        filename = "clt.nc"
        f=cdms2.open(filename)
        self.fclt = f('clt')
        self.fGrid = [self.fclt.getLatitude(), self.fclt.getLongitude()]
        g2D = self.fclt.getGrid().toCurveGrid()
        self.grid2D = [g2D.getLatitude()[:], g2D.getLongitude()[:]]

        filename = "era40_tas_sample.nc"
        g=cdms2.open(filename)
        self.gtas = g('tas')
        self.gGrid = [self.gtas.getLatitude(), self.gtas.getLongitude()]
        g2D = self.gtas.getGrid().toCurveGrid()
        self.grid2D = [g2D.getLatitude()[:], g2D.getLongitude()[:]]

        self.gcltVal010101 = 74.0087890625 
        self.ftasVal1010 = 283.39410400390625
        self.soVal1010 = 33.88642120361328
        self.MissingValue = 1.e20
        self.eps = 1e-4

    def test1_Var_Regrid_with_time(self):
        test = 'test var.regrid with time'
        gclt = self.fclt.regrid(self.gtas.getGrid())
        self.assertEqual(gclt[0,...].shape, self.gtas[0,...].shape)
        self.assertEqual(gclt.shape[0], self.fclt.shape[0])
        self.assertLess(abs(gclt.mean()-self.fclt.mean()), 1)

    def test2_tasWithLevelsUsingLibCF(self):
        test = 'Time test using regrid2 gsRegrid'
        r2 = regrid2.GenericRegrid(self.gGrid, self.fGrid, 
                                   dtype = self.gtas.dtype,
                                   regridMethod='linear', 
                                   regridTool='libcf')
        r2.computeWeights()
        fShape = list(self.gtas.shape[:-2]) + list(self.fGrid[0].shape)
        ftas = np.ma.masked_array(np.ones(fShape, self.gtas.dtype)*self.gtas.missing_value,
                                  mask = np.zeros(fShape))
        ftas.missing_value = self.gtas.missing_value
        r2.apply(self.gtas, ftas)
        if False:
            vmin = self.gtas.min()
            vmax = self.gtas.max()
            pylab.subplot(1,4,1)
            pylab.pcolor(self.gtas[-1,...], vmin=vmin, vmax=vmax)
            pylab.colorbar()
            pylab.title('self.gtas[-1,...]')
            pylab.subplot(1,4,2)
            pylab.pcolor(ftasOld[-1,...], vmin=vmin, vmax=vmax)
            pylab.colorbar()
            pylab.title('ftasOld[-1,...]')
            pylab.subplot(1,4,3)
            pylab.pcolor(ftas[0,...], vmin=vmin, vmax=vmax)
            pylab.colorbar()
            pylab.title('ftas[-1,...]')
            pylab.subplot(1,4,4)
            pylab.pcolor(ftas[-1,...] - ftasOld[-1,...], vmin=-1, vmax=1)
            pylab.colorbar()
            pylab.title('ftas[-1,...] - ftasOld[-1,...]')
        
        self.assertEqual(ftas[0,...].shape, self.fclt[0,...].shape)
        self.assertEqual(ftas.shape[0], self.gtas.shape[0])
        self.assertGreater(abs(ftas[ftas>0].mean()-self.gtas.mean()), 1)

    def test3_use_gsRegrid(self):
        test = 'test 1 - using gsRegrid'
        ro = regrid2.gsRegrid.Regrid(self.gGrid, self.fGrid)
        ro.computeWeights(20, .01*(180./(46)))
        ftas = np.zeros(self.fclt[0,...].shape, self.gtas.dtype)
        ro.apply(self.gtas[0,...], ftas)
        gtas0 = self.gtas[0,...]
        self.assertLess(abs(ftas[ftas>0].mean()-gtas0.mean()), 1)

    def test4_gsRegrid_Masking(self):
        # Masking
        test = 'test mask attached to data for gsRegrid'
        r3 = regrid2.GenericRegrid(self.hGrid, self.fGrid,
                                   dtype = self.hso.dtype,
                                   regridMethod='linear', 
                                   regridTool='libcf')
        r3.computeWeights()
        gShape = list(self.hso.shape[:-2]) + list(self.fGrid[0].shape)
        gso = np.ma.masked_array(np.zeros(gShape, self.hso.dtype), mask = np.zeros(gShape))
        r3.apply(self.hso, gso)
        maxv = gso <= self.hso.max()+1
        aso = gso > 0
        bb = (np.array(maxv,np.int32) + np.array(aso,np.int32))==2
        self.assertLess(abs(gso[bb].mean()-self.hso.mean()), 1)
        self.assertFalse(gso.mask[10,10])
        self.assertFalse(gso.mask[3,3])

    def test5_gsRegrid_set_mask_before_regridding(self):
        test = 'test set mask and compute before regridding'
        so = self.hso
        sGrid = [so.getLatitude(), so.getLongitude()]
        r4 = regrid2.GenericRegrid(self.hGrid, self.fGrid,
                                   dtype = so.dtype,
                                   regridMethod = 'linear',
                                   regridTool = 'libcf',
                                   srcGridMask = self.hso.mask)
        r4.computeWeights()
        gShape = list(self.hso.shape[:-2]) + list(self.fGrid[0].shape)
        gso = np.ma.masked_array(np.zeros(gShape, self.hso.dtype), mask = np.zeros(gShape))
        r4.apply(so, gso)
        maxv = gso <= self.hso.max()+1
        aso = gso > 0
        bb = (np.array(maxv,np.int32) + np.array(aso,np.int32))==2
        self.assertLess(gso[bb].mean()-self.hso.mean(), 1)
        self.assertFalse(gso.mask[10,10])
        self.assertFalse(gso.mask[3,3])


if __name__ == '__main__':
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(TestTimeRegridding)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


