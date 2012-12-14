import cdms2
import numpy
import unittest
import ESMP
from matplotlib import pylab
import sys

class TestTasRegrid(unittest.TestCase):
    """
    All test interpolate to the same grid
    """

    def setUp(self):
        pass

    def test1_regrid(self):
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0,...]
        ta = cdms2.open(sys.prefix + '/sample_data/ta_ncep_87-6-88-4.nc')('ta')[0, 0,...]
        diag = {}
        cltInterp = clt.regrid( ta.getGrid(), 
                                regridTool = 'libcf', 
                                mkCyclic = True, verbose = True,
                                diag = diag )
        print cltInterp.sum()
        n = reduce(lambda x,y: x*y, cltInterp.shape)
        self.assertLess(abs(cltInterp.sum() - 696921.0)/n, 0.3)
        if False:
            pylab.pcolor(ta.getLongitude()[:], ta.getLatitude()[:], cltInterp)
            pylab.colorbar()
            pylab.title('cltInterp')
        self.assertEqual(True, True)

    def test2_mkCyclic(self):
        import regrid2
        y = numpy.array([-90.0 + i*30.0 for i in range(7)])
        x = numpy.array([(i+0.5)*60.0 for i in range(6)])
        yy = regrid2.gsRegrid.getTensorProduct(y, 0, [len(y), len(x)])
        xx = regrid2.gsRegrid.getTensorProduct(x, 1, [len(y), len(x)])
        coords = [yy, xx]
        dims = [len(y), len(x)]
        newCoords, newDims = regrid2.gsRegrid.makeCoordsCyclic(coords, dims)
        self.assertEqual(newDims[-1], dims[-1] + 1)
        
if __name__ == '__main__':
    print "" # Spacer
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(TestTasRegrid)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()

