import cdms2
import numpy
import unittest
from matplotlib import pylab as pl
from mpl_toolkits.basemap import Basemap as bm
from mpi4py import MPI
import sys

PLOT = False

class TestTasRegrid(unittest.TestCase):
    """
    All test interpolate to the same grid
    """

    def setUp(self):
        self.rootPe = 0
        self.pe = MPI.COMM_WORLD.Get_rank()
        self.clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, ...]
#        self.tas = cdms2.open(sys.prefix + \
#                       'tas_ccsr-95a_1979.01-1979.12.nc')('tas')[0, 0,...]
        self.tas = cdms2.open(sys.prefix + \
                                  '/sample_data/tas_ecm_1979.nc')('tas')[0, ...]

        if PLOT:
            lllat = self.clt.getLatitude()[:].min()
            urlat = self.clt.getLatitude()[:].max()
            lllon = self.clt.getLongitude()[:].min()
            urlon = self.clt.getLongitude()[:].max()

            self.cmap = bm(llcrnrlat = lllat, llcrnrlon = lllon, 
                           urcrnrlat = urlat, urcrnrlon = urlon,
                           resolution = 'i', projection = 'cyl')

            lllat = self.tas.getLatitude()[:].min()
            urlat = self.tas.getLatitude()[:].max()
            lllon = self.tas.getLongitude()[:].min()
            urlon = self.tas.getLongitude()[:].max()

            self.tmap = bm(llcrnrlat = lllat, llcrnrlon = lllon, 
                           urcrnrlat = urlat, urcrnrlon = urlon,
                           resolution = 'i', projection = 'cyl')

    def test_test1(self):
        """
        2D
        """
        tas = cdms2.open(sys.prefix + \
                             '/sample_data/tas_ccsr-95a_1979.01-1979.12.nc')('tas')[0, 0,...]
        tasInterp = tas.regrid( tas.getGrid() )
        print numpy.all(tasInterp.mask)
        if not numpy.all(tasInterp.mask):
            n = reduce(lambda x,y: x*y, tasInterp.shape)
            diff = abs(numpy.sum(tas - tasInterp))/float(n)
            self.assertLess(diff, 3.e-5)
        
    def test_test2(self):
        """
        2D + time
        """
        tas = cdms2.open(sys.prefix + \
                             '/sample_data/tas_ccsr-95a_1979.01-1979.12.nc')('tas')[:, 0,...]
        tasInterp = tas.regrid( tas.getGrid() )
        if not numpy.all(tasInterp.mask):
            n = reduce(lambda x,y: x*y, tasInterp.shape)
            diff = abs(numpy.sum(tas - tasInterp))/float(n)
            self.assertLess(diff, 3.e-5)
        
    def test_test3(self):
        """
        2D + level
        """
        tas = cdms2.open(sys.prefix + \
                             '/sample_data/tas_ccsr-95a_1979.01-1979.12.nc')('tas')[0, :,...]
        tasInterp = tas.regrid( tas.getGrid() )
        if not numpy.all(tasInterp.mask):
            n = reduce(lambda x,y: x*y, tasInterp.shape)
            diff = abs(numpy.sum(tas - tasInterp))/float(n)
            self.assertLess(diff, 3.e-5)
        
    def test_test4(self):
        """
        2D + level + time
        """
        tas = cdms2.open(sys.prefix + \
                             '/sample_data/tas_ccsr-95a_1979.01-1979.12.nc')('tas')[:, :,...]
        tasInterp = tas.regrid( tas.getGrid() )
        if not numpy.all(tasInterp.mask):
            tasInterp[0,0,...]
            n = reduce(lambda x,y: x*y, tasInterp.shape)
            diff = abs(numpy.sum(tas - tasInterp))/float(n)
            self.assertLess(diff, 3.e-5)

    def Xtest_test5(self):
        tasInterp = self.tas.regrid(self.clt.getGrid())
        cltInterp = self.clt.regrid(self.tas.getGrid())
        tasIntCyc = self.tas.regrid(self.clt.getGrid(), mkCyclic = True)
        cltIntCyc = self.clt.regrid(self.tas.getGrid(), mkCyclic = True)
        
        if PLOT:
            fig = pl.figure(1)
            fig.add_subplot(2,2,1)
            self.cmap.pcolor(self.tas.getLongitude()[:], self.tas.getLatitude()[:],
                      cltInterp, vmin = 0, vmax = 100)
            self.cmap.colorbar()
            self.cmap.drawcoastlines()
            pl.title("clt Interp")
            
            fig = pl.figure(1)
            fig.add_subplot(2,2,2)
            self.cmap.pcolor(self.tas.getLongitude()[:], self.tas.getLatitude()[:],
                      cltIntCyc, vmin = 0, vmax = 100)
            self.cmap.colorbar()
            self.cmap.drawcoastlines()
            pl.title("clt Interp Cyclic")

            fig.add_subplot(2,2,3)
            self.tmap.pcolor(self.clt.getLongitude()[:], self.clt.getLatitude()[:],
                      tasInterp, vmin = 250, vmax = 300)
            self.tmap.colorbar()
            self.tmap.drawcoastlines()
            pl.title("tas Interp")
            
            fig.add_subplot(2,2,4)
            self.tmap.pcolor(self.clt.getLongitude()[:], self.clt.getLatitude()[:],
                      tasIntCyc, vmin = 250, vmax = 300)
            self.tmap.colorbar()
            self.tmap.drawcoastlines()
            pl.title("tas Interp Cyclic")
        
if __name__ == '__main__':
    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(TestTasRegrid)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    if PLOT: pl.show()
