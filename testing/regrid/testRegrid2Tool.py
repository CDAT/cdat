"""
$Id: testEsmfVsLibcf.py 2204 2012-06-01 02:25:42Z pletzer $

Unit tests comparing esmf vs libcf

"""
import operator
import numpy
import cdms2
import unittest
import time
import copy
import sys

PLOT = False

class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test_1(self):
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc')
        clt = f('clt')[0, :, :]
        cltInterp = clt.regrid(clt.getGrid(), regridTool='regrid2')
        self.assertLess(numpy.sum(clt - cltInterp), 1.e-3)

    def test_2(self):
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc')
        clt = f('clt')[:, :, :]
        cltInterp = clt.regrid(clt.getGrid(), regridTool='regrid2')
        self.assertLess(numpy.sum(clt - cltInterp), 5.e-3)

    def test_3(self):
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc')
        clt = f('clt')[:, :, :]
        u = f('u')[:, :, :]
        cltInterp = clt.regrid(u.getGrid(), regridTool='regrid2')
        avgClt = clt.sum()/float(reduce(operator.mul, clt.shape))
        avgCltInterp = cltInterp.sum()/float(reduce(operator.mul, cltInterp.shape))
        self.assertLess(abs(avgClt - avgCltInterp), 0.3)

if __name__ == '__main__':
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)

