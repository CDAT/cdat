"""
$Id: testSo2Tas.py 2310 2012-06-28 16:49:44Z dkindig $

Test diagnostics

"""

import re
import numpy
import cdms2
import regrid2
import unittest
import ESMP
from regrid2 import esmf
from matplotlib import pylab
from mpi4py import MPI
import types
import sys


class Test(unittest.TestCase):

    def setUp(self):
        self.pe = MPI.COMM_WORLD.Get_rank()
        self.nprocs = MPI.COMM_WORLD.Get_size()

        self.so = cdms2.open(sys.prefix + \
                                 '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')('so')
        self.tasGood = cdms2.open(sys.prefix + \
                                      '/sample_data/tas_Amon_HadGEM2-A_amip_r1i2p1_197809-200811_2timesteps.nc')('tas')
        self.tasBad = cdms2.open(sys.prefix + \
                                     '/sample_data/tas_Amon_ACCESS1-0_historical_r1i1p1_185001-189912_2timesteps.nc')('tas')

    def test1_esmf_conserve_bad(self):
        print "This test should fail"
        diag = {}
        soInterp = self.so[0,0,...].regrid(self.tasBad.getGrid(), regridTool='esmf', 
                                           regridMethod='conserve', 
                                           coordSys='cart', diag = diag)

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


