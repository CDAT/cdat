import unittest
import cdms2
import ESMP
import regrid2
import numpy
import sys

class TestLevelRegridLibCfAndESMP(unittest.TestCase):
    def setUp(self):
        fso = cdms2.open(sys.prefix + \
                             '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        self.soLevel = fso('so')[0,0:5,...]
        self.soTime = fso('so')[:,0,...]
        self.so = fso('so')[:,0:5,...]

        fclt = cdms2.open(sys.prefix + 'sample_data/clt.nc')
        self.clt = fclt('clt')

    def test_ESMPTimeLevel2D(self):
        soInterp = self.so.regrid(self.clt.getGrid(), regridTool = 'ESMP')
        print soInterp.shape
        soInterpInterp = soInterp.regrid(self.so.getGrid(), regridTool = 'ESMP')

        ntot = numpy.array(self.so.shape).prod()
        avgdiff = numpy.sum(self.so - soInterpInterp) / float(ntot)
        soInterp.toVisit('soInterp.vsh5')
        #soInterpInterp.toVisit('soInterpInterp.vsh5')
        #avgdiff.toVisit('soAvgDiff.vsh5')
        self.assertLess(abs(avgdiff), 2e18)

    def test_ESMPTime2D(self):
        soInterp = self.soTime.regrid(self.clt.getGrid(), regridTool = 'ESMP')
        soInterpInterp = soInterp.regrid(self.soTime.getGrid(), regridTool = 'ESMP')

        ntot = numpy.array(self.soTime.shape).prod()
        avgdiff = numpy.sum(self.soTime - soInterpInterp) / float(ntot)
        soInterp.toVisit('soInterpTime.vsh5')
        #soInterpInterp.toVisit('soInterpInterp.vsh5')
        #avgdiff.toVisit('soAvgDiff.vsh5')
        self.assertLess(abs(avgdiff), 2e18)

    def test_ESMPLevel2D(self):
        soInterp = self.soLevel.regrid(self.clt.getGrid(), regridTool = 'ESMP')
        soInterpInterp = soInterp.regrid(self.soLevel.getGrid(), regridTool = 'ESMP')

        ntot = numpy.array(self.soLevel.shape).prod()
        avgdiff = numpy.sum(self.soLevel - soInterpInterp) / float(ntot)
        soInterp.toVisit('soInterpLevel.vsh5')
        #soInterpInterp.toVisit('soInterpInterp.vsh5')
        #avgdiff.toVisit('soAvgDiff.vsh5')
        self.assertLess(abs(avgdiff), 2e18)

if __name__ == "__main__":
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(TestLevelRegridLibCfAndESMP)
    unittest.TextTestRunner(verbosity = 1).run(suite)

