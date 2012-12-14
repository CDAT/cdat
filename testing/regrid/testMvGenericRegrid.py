"""
Test mvGenericRegrid class
$Id: testMvGenericRegrid.py 2354 2012-07-11 15:28:14Z pletzer $
"""

import cdms2
import numpy
import unittest
import regrid2
import ESMP
import matplotlib.pylab as pl
from mpi4py import MPI
import sys

class TestMvGenericRegrid(unittest.TestCase):
    """
    All test interpolate to the same grid
    """

    def setUp(self):
        """
        Set up the grids to pass to mvGenericRegrid
        """
        self.doPlots = False
        self.clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, ...]
        # Convert to curvilinear
        cds, cds_forBounds = [], []
        cds.append(self.clt.getLatitude())
        cds.append(self.clt.getLongitude())
        cds_forBounds.append(self.clt.getLatitude())
        cds_forBounds.append(self.clt.getLongitude())
        self.cltGrid, nDims = regrid2.gsRegrid.makeCurvilinear(cds)
        self.cltInterp = numpy.array(self.clt) * 0.0 + self.clt.missing_value
        self.cltInterpInterp = numpy.array(self.clt) * 0.0 + self.clt.missing_value

        # Salinity check
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        self.so = f('so')
        soGrid = []
        soGrid.append(self.so.getLatitude())
        soGrid.append(self.so.getLongitude())
        self.soGrid = soGrid
        self.soInterp = numpy.array(self.clt) * 0.0 + self.so.missing_value

        self.tol = 1e2
        self.comm = MPI.COMM_WORLD
        self.rank = self.comm.Get_rank()
        self.size = self.comm.Get_size()

    def test0_mvGeneric_dstMaskFloat_salinity(self):
        """
        Check that the number of returned masked values is ok 
        """
        ro = regrid2.GenericRegrid(self.soGrid, self.cltGrid, dtype = self.so.dtype,
                                   regridMethod='linear', regridTool='libcf')
        ro.computeWeights()
        ro.apply(self.so[0,0,:,:], self.soInterp, missingValue = self.so.missing_value)

        self.assertLess(abs(self.so[0,0,...].mask.sum()/float(self.so[0,0,...].size) -  0.35), 
                        0.05)
        soInterpMask = numpy.array(self.soInterp == self.so.missing_value, 
                                   numpy.int64)
        self.assertLess(abs(soInterpMask.sum()/float(soInterpMask.size) -  0.39), 
                        0.05)

    def test1_LibCF_clt(self):
        """
        Out and back, same grid using mvGenericRegrid -> LibCF, linear
        """
        ro = regrid2.GenericRegrid(self.cltGrid, self.cltGrid, self.clt.dtype,
                                   regridMethod='linear', regridTool='libcf')
        ro.computeWeights()

        ro.apply(self.clt, self.cltInterp)
        ro.apply(self.cltInterp, self.cltInterpInterp)
        nCell = numpy.array(self.clt.shape).prod()
        avgDiffInterp = (abs(self.clt - self.cltInterp)).sum()/float(nCell)
        avgDiffInterpInterp = abs(self.clt - self.cltInterpInterp).sum()/float(nCell)
        self.assertLess(avgDiffInterp, self.tol)
        self.assertLess(avgDiffInterpInterp, self.tol)

        if self.rank == 0:
            avgDiffInterp = (abs(self.clt - self.cltInterp)).sum()/float(nCell)
            avgDiffInterpInterp = abs(self.clt - self.cltInterpInterp).sum()/float(nCell)
            self.assertLess(avgDiffInterp, self.tol)
            self.assertLess(avgDiffInterpInterp, self.tol)
    
            if False:
                pl.figure(1)
                pl.subplot(3,2,1)
                pl.pcolor(self.cltGrid[1], self.cltGrid[0], self.clt)
                pl.title('clt')
                pl.colorbar()
                pl.subplot(3,2,2)
                pl.pcolor(self.cltGrid[1], self.cltGrid[0], self.cltInterp,
                        vmin = 0, vmax = 100)
                pl.title('Interp')
                pl.colorbar()
                pl.subplot(3,2,3)
                pl.pcolor(self.cltGrid[1], self.cltGrid[0], self.cltInterpInterp,
                        vmin = 0, vmax = 100)
                pl.title('InterpInterp')
                pl.colorbar()
                pl.subplot(3,2,4)
                pl.pcolor(self.cltGrid[1],self.cltGrid[0], self.clt-self.cltInterp)
                pl.colorbar()
                pl.title('clt-cltInterp')
                pl.subplot(3,2,5)
                pl.pcolor(self.cltGrid[1],self.cltGrid[0], self.clt-self.cltInterpInterp)
                pl.colorbar()
                pl.title('clt-cltInterpInterp')
                pl.subplot(3,2,6)
                pl.pcolor(self.cltGrid[1],self.cltGrid[0], self.cltGrid[1])
                pl.colorbar()
                pl.title('Longitude')
                per = 0
                string0 = "LibCF coordSys = Bilinear, "
                string1 = "periodicity = %d, " % (per)
                string2 = "MPI COMM size = %d" % (self.size)
                pl.suptitle(string0 + string1 + string2)

    def test2_ESMF_clt(self):
        """
        Out and back, same grid using mvGenericRegrid -> ESMF, linear
        """
        per = 1
        coordSys = 'spherical degrees'
        grid = [self.cltGrid[0], self.cltGrid[1]]
        ro = regrid2.GenericRegrid(grid, grid, 
                                   dtype=self.clt.dtype,
                                   regridMethod='linear', 
                                   regridTool = 'esMf', periodicity = per,
                                   coordSys = coordSys)
        ro.computeWeights()
        ro.apply(numpy.array(self.clt), self.cltInterp, rootPe = 0)
        self.cltInterp = self.comm.bcast(self.cltInterp, root = 0)
        ro.apply(self.cltInterp, self.cltInterpInterp, rootPe = 0)
        nCell = numpy.array(self.clt.shape).prod()

        if self.rank == 0:
            avgDiffInterp = (abs(self.clt - self.cltInterp)).sum()/float(nCell)
            avgDiffInterpInterp = abs(self.clt - self.cltInterpInterp).sum()/float(nCell)
            self.assertLess(avgDiffInterp, self.tol)
            if self.size > 1:
                self.assertLess(avgDiffInterpInterp, 600)
            else:
                self.assertLess(avgDiffInterpInterp, self.tol)

            if False:
                pl.figure(2)
                pl.subplot(3,2,1)
                pl.pcolor(grid[1], grid[0], self.clt)
                pl.title('clt')
                pl.colorbar()
                pl.subplot(3,2,2)
                pl.pcolor(grid[1], grid[0], self.cltInterp,
                        vmin = 0, vmax = 100)
                pl.title('Interp')
                pl.colorbar()
                pl.subplot(3,2,3)
                pl.pcolor(grid[1], grid[0], self.cltInterpInterp,
                        vmin = 0, vmax = 100)
                pl.title('InterpInterp')
                pl.colorbar()
                pl.subplot(3,2,4)
                pl.pcolor(grid[1],grid[0], self.clt-self.cltInterp)
                pl.colorbar()
                pl.title('clt-cltInterp')
                pl.subplot(3,2,5)
                pl.pcolor(grid[1],grid[0], self.clt-self.cltInterpInterp)
                pl.colorbar()
                pl.title('clt-cltInterpInterp')
                pl.subplot(3,2,6)
                pl.pcolor(grid[1],grid[0], grid[1])
                pl.colorbar()
                pl.title('Longitude')
                string0 = "ESMF coordSys = %s, " % coordSys
                string1 = "periodicity = %d, " % (per)
                string2 = "MPI COMM size = %d" % (self.size)
                pl.suptitle(string0 + string1 + string2)

    def test3_ESMF_Masking(self):
        """
        Out, ESMF, Masking in __init__, Bilinear
        """
        per = 1
        coordSys = 'spherical degrees'
        grid = [self.cltGrid[0], self.cltGrid[1]]
        mask = numpy.array(self.clt > 90, dtype = numpy.int32)
        ro = regrid2.GenericRegrid(grid, grid, self.clt.dtype, 
                               regridMethod = 'linear',
                               regridTool = 'esMf', periodicity = per,
                               coordSys = coordSys, srcGridMask = mask)
        ro.computeWeights()
        ro.apply(numpy.array(self.clt), self.cltInterp, rootPe = 0)
        nCell = numpy.array(self.clt.shape).prod()

        if self.rank == 0:
            avgDiffInterp = (abs(self.clt - self.cltInterp)).sum()/float(nCell)
            avgDiffInterpInterp = abs(self.clt - self.cltInterpInterp).sum()/float(nCell)
            # we're expecting some ver large values because of the masking
            #self.assertLess(avgDiffInterp, 50)

            if False:
                pl.figure(3)
                pl.subplot(1, 3, 1)
                pl.pcolor(grid[1], grid[0], self.clt)
                pl.title('clt')
                pl.colorbar()
                pl.subplot(1, 3, 2)
                pl.pcolor(grid[1], grid[0], self.cltInterp,
                        vmin = 0, vmax = 100)
                pl.title('Interp')
                pl.colorbar()
                pl.subplot(1, 3, 3)
                pl.pcolor(grid[1], grid[0], self.clt-self.cltInterp)
                pl.colorbar()
                pl.title('clt-cltInterp')
                string0 = "ESMF coordSys = %s, " % coordSys
                string1 = "periodicity = %d, " % (per)
                string2 = "MPI COMM size = %d" % (self.size)
                pl.suptitle(string0 + string1 + string2)

    def Xtest4_ESMF_Conservative_2D_clt(self):
        """
        Out, ESMF, Conservative metric
        """
        per = 1
        coordSys = 'spherical degrees'
        grid = [self.cltGrid[0], self.cltGrid[1]]
        mask = numpy.array(self.clt > 90, dtype = numpy.int32)
        newclt = numpy.ones(self.clt.shape) * self.clt
        newclt[numpy.where(self.clt>75)] = self.clt.missing_value
        ro = regrid2.GenericRegrid(grid, grid, self.clt.dtype, 
                                   regridMethod = 'conserv',
                                   regridTool = 'esMf', 
                                   periodicity = per,
                                   coordSys = coordSys)
        ro.computeWeights()
        print dir(ro.computeWeights())
        ro.apply(numpy.array(newclt), self.cltInterp, 
                 srcMissingValue = self.clt.missing_value, rootPe = 0)
      
        nCell = numpy.array(self.clt.shape).prod()

        if self.rank == 0:
            avgDiffInterp = (abs(self.clt - self.cltInterp)).sum()/float(nCell)
            avgDiffInterpInterp = abs(self.clt - self.cltInterpInterp).sum()/float(nCell)
            #self.assertLess(avgDiffInterp, 50)

            if True:
                pl.figure(4)
                pl.subplot(3,2,1)
                pl.pcolor(grid[1], grid[0], self.clt)
                pl.title('clt')
                pl.colorbar()
                pl.subplot(3,2,2)
                pl.pcolor(grid[1], grid[0], (newclt == self.clt.missing_value)+mask,
                        vmin = 0, vmax = 2)
                pl.title('newclt == self.clt.missing_value')
                pl.colorbar()
                pl.subplot(3,2,3)
                pl.pcolor(grid[1], grid[0], self.cltInterp)
                mn, mx = self.cltInterp.min(), self.cltInterp.max()
                pl.title('newMask %5.2f %5.2f' % (mn,mx))
                pl.colorbar()
                pl.subplot(3,2,4)
                pl.pcolor(grid[1],grid[0], mask+self.cltInterp)
                pl.colorbar()
                pl.title('mask')
                pl.subplot(3,2,5)
                pl.pcolor(grid[1],grid[0], mask)
                pl.colorbar()
                pl.title('(newclt==self.clt.missing_value) - self.cltInterp')
                pl.subplot(3,2,6)
                pl.pcolor(grid[1],grid[0], newclt == self.clt.missing_value)
                pl.colorbar()
                pl.title('newclt-cltInterp')
                string0 = "ESMF coordSys = %s, " % coordSys
                string1 = "periodicity = %d, " % (per)
                string2 = "MPI COMM size = %d" % (self.size)
                pl.suptitle(string0 + string1 + string2)

    def test5_LibCF_LevelTime(self):
        """
        Interpolate over one level/time
        """
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc')
        clt = f('clt')
        v = f('v')
        
        # mask
        srcGridMask = numpy.array(v[0,0,...] == v.missing_value, numpy.int32)

        # v onto the ctl grid
        srcGrd, srcNDims = regrid2.gsRegrid.makeCurvilinear([v.getLatitude(),
                                                           v.getLongitude()])
        dstGrd, dstNDims = regrid2.gsRegrid.makeCurvilinear([clt.getLatitude(), 
                                                           clt.getLongitude()])
        
        ro = regrid2.GenericRegrid(srcGrd, dstGrd, 
                                   clt.dtype,
                                   regridMethod = 'linear',
                                   regridTool = 'esmf',
                                   periodicity = 1,
                                   coordSys = 'cart',
                                   srcGridMask = srcGridMask)
        ro.computeWeights()
        
        vInterp = numpy.ones(clt.shape[-2:],
                              v.dtype) * v.missing_value
        ro.apply(numpy.array(v[0,0,...]), vInterp, rootPe = 0)
        
        print 'min/max of v: %f %f' % (v.min(), v.max())
        print 'min/max of vInterp: %f %f' % (vInterp.min(), vInterp.max())

        if False:
            pl.figure()
            pl.subplot(1,2,1)
            pl.pcolor(srcGrd[1], srcGrd[0], v[0, 0,...], vmin=-20, vmax=20)
            pl.title('test5: v[0, 0,...]')
            pl.colorbar()
            pl.subplot(1,2,2)
            pl.pcolor(dstGrd[1], dstGrd[0], vInterp, vmin=-20, vmax=20)
            pl.title('test5: vInterp')
            pl.colorbar()

    def Xtest6_ESMF_Conserve_LevelTime_clt(self):
        """
        Interpolate over level/time in addition to lat-lon
        """
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc')
        clt = f('clt')
        v = f('v')
        
        # mask
        srcGridMask = numpy.array(v[0,0,...] == v.missing_value, numpy.int32)

        # v onto the ctl grid
        srcGrd, srcNDims = regrid2.gsRegrid.makeCurvilinear([v.getLatitude(),
                                                           v.getLongitude()])
        dstGrd, dstNDims = regrid2.gsRegrid.makeCurvilinear([clt.getLatitude(), 
                                                           clt.getLongitude()])
        
        ro = regrid2.GenericRegrid(srcGrd, dstGrd, 
                                   regridMethod = 'conserve',
                                   regridTool = 'esmp',
                                   periodicity = 1,
                                   srcGridMask = srcGridMask)
        ro.computeWeights()
        
        vInterp = numpy.ones(list(v.shape[:-2]) + list(clt.shape[-2:]),
                              v.dtype) * v.missing_value
        ro.apply(numpy.array(v), vInterp, rootPe = 0)
        
        print 'min/max of v: %f %f' % (v.min(), v.max())
        print 'min/max of vInterp: %f %f' % (vInterp.min(), vInterp.max())

        if False:
            nTimes = v.shape[0]
            nLevels = v.shape[1]
            for el in range(nTimes):
                for k in range(nLevels):
                    pl.figure()
                    pl.subplot(1,2,1)
                    pl.pcolor(srcGrd[1], srcGrd[0], v[el, k,...], vmin=-20, vmax=20)
                    pl.title('test6: v[%d, %d,...]' % (el, k))
                    pl.colorbar()
                    pl.subplot(1,2,2)
                    pl.pcolor(dstGrd[1], dstGrd[0], vInterp[el, k,...], vmin=-20, vmax=20)
                    pl.title('test6: vInterp[%d, %d,...]' % (el, k))
                    pl.colorbar()
                    


    
if __name__ == '__main__':
    print "" # Spacer
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(TestMvGenericRegrid)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pl.show()


