import cdms2
import numpy
import unittest
import ESMP
import sys

PRINT = False
PLOT = False

class TestCltRegrid(unittest.TestCase):
    """
    All test interpolate to the same grid
    """

    def setUp(self):
        self.tolerance = 2.e-6

    def Xtest1(self):
        """
        2D
        """
        u = cdms2.open(sys.prefix + '/sample_data/clt.nc')('u')[0, 0,...]
        uInterp = u.regrid( u.getGrid() )
        n = reduce(lambda x,y: x*y, uInterp.shape)
        diff = abs(numpy.sum(u - uInterp))/float(n)
        nans = numpy.isnan(uInterp).sum()
        self.assertEqual(numpy.isnan(u).sum(), 0)
        self.assertEqual(numpy.isnan(uInterp).sum(), 0)
        self.assertLess((uInterp.mean()-u.mean()), self.tolerance)
        self.assertLess(diff, self.tolerance)

    def Xtest2(self):
        """
        2D + time
        """
        u = cdms2.open(sys.prefix + '/sample_data/clt.nc')('u')[:, 0,...]
        uInterp = u.regrid( u.getGrid() )
        self.assertEqual(numpy.isnan(u).sum(), 0)
        self.assertEqual(numpy.isnan(uInterp).sum(), 0)
        n = reduce(lambda x,y: x*y, uInterp.shape)
        diff = abs(numpy.sum(u - uInterp))/float(n)
        success = False
        self.assertLess(diff, self.tolerance)

    def test3(self):
        """
        2D + level
        """
        print 'This is a known failure for now. ESMF are looking into the error'
        u = cdms2.open(sys.prefix + '/sample_data/clt.nc')('u')[0, :,...]
        uCart = u.regrid( u.getGrid(),
                          regridTool='esmf', regridMethod='linear',
                          coordSys = 'cart',
                          periodicity = 1)
        uDegr = u.regrid( u.getGrid(),
                          regridTool='esmf', regridMethod='linear',
                          coordSys = 'deg',
                          periodicity = 1)
        n = reduce(lambda x,y: x*y, uCart.shape)
        mask = (u == u.missing_value)
        if PLOT:
            import matplotlib.pylab as pl
            fig = pl.figure()
            fig.add_subplot(2,1,1)
            pl.pcolor(u.getLongitude()[:], u.getLatitude()[:], uCart[1,...],
                      vmin = 0)
            pl.colorbar()
            pl.title('Cartiesian')
            fig.add_subplot(2,1,2)
            pl.pcolor(u.getLongitude()[:], u.getLatitude()[:], uDegr[1,...],
                      vmin = 0)
            pl.colorbar()
            pl.title('Degrees')
            pl.show()
        if PRINT:
            print
            print 'Level 0, u.min() = %11.3f, uInterp.min() = %11.3f' % \
                     (u[0,...].min(), uInterp[0,...].min())
            print 'Level 1, u.min() = %11.3f, uInterp.min() = %11.3f' % \
                     (u[1,...].min(), uInterp[1,...].min())

            print '\nFor indices 52,59 and 60, 68'
            print 'm means missing'
            print 'd means uInterp < u'
            print '. means uInterp == u'
            for i in range(52, 59):
                string = ""
                for j in range(60, 68):
                    if uInterp.mask[1,i,j] == True:
                        string = string+"m"
                    elif (uInterp[1,i,j] < u[1,i,j]):
                        string = string+"d"
                    else:
                        string = string + "."
                print string
            print

        diff = abs(numpy.sum(u*(1-mask) - uCart)/float(n))
        self.assertLess(diff, 1.e-3)
        diff = abs(numpy.sum(u*(1-mask) - uDegr)/float(n))
        self.assertLess(diff, 1.e-3)

    def Xtest4(self):
        """
        2D + level + time
        """
        u = cdms2.open(sys.prefix + 'sample_data/clt.nc')('u')[:, :,...]
        uInterp = u.regrid( u.getGrid(),
                            regridTool='esmf', regridMethod='linear',
                            coordSys = 'cart',
                            periodicity = 1)
        n = reduce(lambda x,y: x*y, uInterp.shape)
        mask = (u == u.missing_value)
        diff = abs(numpy.sum(u*(1-mask) - uInterp)/float(n))
        self.assertLess(diff, 1.e-3)

if __name__ == '__main__':
    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(TestCltRegrid)
    unittest.TextTestRunner(verbosity = 1).run(suite)
