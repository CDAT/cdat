"""
$Id: testSo2Tas.py 2356 2012-07-11 15:54:35Z pletzer $

"""

import re
import numpy
import cdms2
import regrid2
import unittest
import ESMP
from regrid2 import esmf
from matplotlib import pylab as pl
from mpi4py import MPI
import types
import sys

Plot = False

class Test(unittest.TestCase):

    def setUp(self):
        self.pe = MPI.COMM_WORLD.Get_rank()
        self.nprocs = MPI.COMM_WORLD.Get_size()

        access = cdms2.open(sys.prefix + \
                                '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        self.access = access('so')[0, 0, ...]
        self.tasGood = cdms2.open(sys.prefix + \
                                      '/sample_data/tas_Amon_HadGEM2-A_amip_r1i2p1_197809-200811_2timesteps.nc')('tas')
        giss = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_GISS-E2-R_historicalNat_r5i1p1_185001-187512_2timesteps.nc')
        self.giss = giss('so')[0,0,...]

    def test1_regrid(self):
        diag = {}
        soInterp = self.access.regrid(self.tasGood.getGrid(), regridTool='regrid', 
                                           diag = diag)
        self.assertLess(soInterp.max(), 40.0)
        self.assertGreater(soInterp.min(), 0.0)

    def test2_libcf(self):
        diag = {}
        soInterp = self.access.regrid(self.tasGood.getGrid(), regridTool='libcf', 
                                           diag = diag)
        self.assertLess(soInterp.max(), 40.0)
        self.assertGreater(soInterp.min(), 0.0)

    def test3_esmf_linear(self):
        diag = {}
        soInterp = self.access.regrid(self.tasGood.getGrid(), regridTool='esmf', 
                                           regridMethod='linear',
                                           coordSys='cart', diag = diag)
        self.assertLess(soInterp.max(), 40.0)
        self.assertGreater(soInterp.min(), 0.0)

    def test4_esmf_conserve_tripolar(self):
        diag = {}
        soInterp = self.access.regrid(self.tasGood.getGrid(), regridTool='esmf', 
                                           regridMethod='conserve', 
                                           coordSys='degr', diag = diag)
        self.assertLess(soInterp.max(), 40.0)

    def test5_esmf_conserve(self):
        diagDeg = {'srcAreaFractions':None, 'srcAreas':None, 'dstAreas':None}
        diagCar = {'srcAreaFractions':None, 'srcAreas':None, 'dstAreas':None}
        soDegree = self.giss.regrid(self.tasGood.getGrid(), regridTool='esmf', 
                                           regridMethod='conserve', 
                                           coordSys='degrees', diag = diagDeg)
        soCartes = self.giss.regrid(self.tasGood.getGrid(), regridTool='esmf', 
                                           regridMethod='conserve', 
                                           coordSys='cart', diag = diagCar)
        smDeg = (self.giss * diagDeg['srcAreaFractions'] * diagDeg['srcAreas']).sum()
        dmDeg = (self.giss * diagDeg['srcAreaFractions'] * diagDeg['srcAreas']).sum()
        smCar = (self.giss * diagCar['srcAreaFractions'] * diagCar['srcAreas']).sum()
        dmCar = (self.giss * diagCar['srcAreaFractions'] * diagCar['srcAreas']).sum()

        if self.pe == 0:
            self.assertLess(abs(smDeg - dmDeg), 1.e-5)
            self.assertLess(abs(smCar - dmCar), 1.e-5)

            if Plot:
                fig = pl.figure('Giss -> HadGem2')
                fig.add_subplot(2,2,1)
                pl.pcolor(self.giss, vmin = 20, vmax = 40)
                pl.colorbar()
                pl.title('self.giss')
                fig.add_subplot(2,2,2)
                pl.pcolor(soDegree, vmin = 20, vmax = 40)
                pl.colorbar()
                pl.title('soDegree')
                fig.add_subplot(2,2,3)
                pl.pcolor(soCartes, vmin = 20, vmax = 40)
                pl.colorbar()
                pl.title('soCartes')

if __name__ == '__main__':
    print ""

    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pl.show()


