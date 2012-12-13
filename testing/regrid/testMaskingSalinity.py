"""
$Id: testMaskingSalinity.py 2354 2012-07-11 15:28:14Z pletzer $

Unit tests for masking on t,z,y,x

"""

import operator
import numpy
import cdms2
import regrid2.esmf
import regrid2
import unittest
import time
import ESMP
import copy
from matplotlib import pylab as pl
from mpi4py import MPI

class TestMaskingSalinity(unittest.TestCase):
    def setUp(self):
        f = cdms2.open('so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        self.accessModel = f('so')

        h = cdms2.open('so_Omon_HadGEM2-CC_historical_r1i1p1_185912-186911_2timesteps.nc')
        self.hadGEM2Model = h('so')

        self.clt = cdms2.open('clt.nc')('clt')[0, :, :]

        self.eps = 1e-5

    def test1_libcf_3d(self):
        mype = MPI.COMM_WORLD.Get_rank()

        mask = self.accessModel[0,0:5,...].mask
        accessModel = self.accessModel[0,0:5,...]

        soInterp = accessModel.regrid(self.clt.getGrid(), srcMask = mask, 
                             regridTool = 'libcf')
        soInter2 = accessModel.regrid(self.clt.getGrid(), srcMask = mask[0,...], 
                             regridTool = 'libcf')
        soInter3 = accessModel.regrid(self.clt.getGrid(), 
                             regridTool = 'libcf')

        if mype == 0:
            aa = (accessModel.min(), accessModel.max())
            bb = (soInterp.min(), soInterp.max())
            cc = (soInter2.min(), soInter2.max())
            dd = (soInter3.min(), soInter3.max())
            self.assertLess(aa[0], bb[0])
            self.assertLess(aa[0], cc[0])
            self.assertLess(aa[0], dd[0])
            self.assertGreater(aa[1], bb[1])
            self.assertGreater(aa[1], cc[1])
            self.assertGreater(aa[1], dd[1])

    def test2_esmf_3d(self):
        mype = MPI.COMM_WORLD.Get_rank()

        mask = self.accessModel[0,5:10,...].mask
        accessModel   = self.accessModel[0,5:10,...]

        soInterp = accessModel.regrid(self.clt.getGrid(), srcMask = mask, 
                             regridTool = 'esmp')
        soInter2 = accessModel.regrid(self.clt.getGrid(), srcMask = mask[0,...], 
                             regridTool = 'esmp')
        soInter3 = accessModel.regrid(self.clt.getGrid(), 
                             regridTool = 'esmp')

        if mype == 0:
            aa = (accessModel.min(), accessModel.max())
            bb = (soInterp.min(), soInterp.max())
            cc = (soInter2.min(), soInter2.max())
            dd = (soInter3.min(), soInter3.max())
            self.assertEqual(0.0, bb[0])
            self.assertEqual(0.0, cc[0])
            self.assertEqual(0.0, dd[0])
            self.assertGreater(aa[0], bb[0])
            self.assertGreater(aa[0], cc[0])
            self.assertGreater(aa[0], dd[0])
            self.assertGreater(aa[1], bb[1])
            self.assertGreater(aa[1], cc[1])
            self.assertGreater(aa[1], dd[1])

    def test3_esmf__conserve_3d(self):
        mype = MPI.COMM_WORLD.Get_rank()

        mask = self.accessModel[0,0:5,...].mask
        accessModel = self.accessModel[0,0:5,...]

        soInterp = accessModel.regrid(self.clt.getGrid(), srcMask = mask, 
                             regridMethod = 'conserv')
        soInter2 = accessModel.regrid(self.clt.getGrid(), srcMask = mask[0,...], 
                             regridMethod = 'conserv')
        soInter3 = accessModel.regrid(self.clt.getGrid(), 
                             regridMethod = 'conserv')

        if mype == 0:
            aa = (accessModel.min(), accessModel.max())
            bb = (soInterp.min(), soInterp.max())
            cc = (soInter2.min(), soInter2.max())
            dd = (soInter3.min(), soInter3.max())
            self.assertEqual(0.0, bb[0])
            self.assertEqual(0.0, cc[0])
            self.assertEqual(0.0, dd[0])
            self.assertGreater(aa[0], bb[0])
            self.assertGreater(aa[0], cc[0])
            self.assertGreater(aa[0], dd[0])
            self.assertGreater(aa[1], bb[1])
            self.assertGreater(aa[1], cc[1])
            self.assertGreater(aa[1], dd[1])

if __name__ == '__main__':
    ESMP.ESMP_Initialize()
    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(TestMaskingSalinity)
    unittest.TextTestRunner(verbosity = 1).run(suite)


