"""
Testing units creation
"""
import unittest
import cdat_info
import cdms2
import cdutil
import ESMP
import numpy
import sys

class Test(unittest.TestCase):

    def setUp(self):
            pass

    def Xtest0(self):
        """
        Test cdutil.generateLandSeaMask
        """
        f = cdms2.open(cdat_info.get_sampledata_path() + \
                           "/so_Omon_GISS-E2-R_historicalNat_r5i1p1_185001-187512_2timesteps.nc")
        s = f("so")
        print s.shape
        print dir(cdutil.create_landsea_mask)
        # this will call the regrid method
        m = cdutil.generateLandSeaMask(s)

    def test1(self):
        """
        Test cdutil.generateLandSeaMask, using a smaller dataset
        """
        f = cdms2.open(cdat_info.get_sampledata_path() + "/clt.nc")
        s = f("clt")
        print s.shape
        print dir(cdutil.create_landsea_mask)
        # this will call the regrid method
        m = cdutil.generateLandSeaMask(s)


if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    ESMP.ESMP_LogSet(True)
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)

