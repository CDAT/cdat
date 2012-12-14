"""
$Id: testEsmfRegridMpi.py 2354 2012-07-11 15:28:14Z pletzer $

Unit tests for parallel esmf interface

"""

import operator
import numpy
import cdms2
import regrid2.esmf
import regrid2
import unittest
import ESMP
import time
import copy
from mpi4py import MPI
from matplotlib import pylab
import sys

PLOT = False

class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test_2d_esmf(self):
        #print 'running test_2d_esmf...'
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]
        clt = cdms2.open(sys.prefix + 'sample_data/clt.nc')('clt')[0, :, :]
        tic = time.time()
        soInterp = so.regrid(clt.getGrid(), regridTool='ESMF') #, periodicity=1)
        soInterpInterp = soInterp.regrid(so.getGrid(), regridTool='ESMF')
        toc = time.time()
        #print 'time to interpolate (ESMF linear) forward/backward: ', toc - tic

        mype = MPI.COMM_WORLD.Get_rank()
        if mype == 0:
            ntot = reduce(operator.mul, so.shape)
            avgdiff = numpy.sum(so - soInterpInterp) / float(ntot)
            #print 'avgdiff = ', avgdiff
            self.assertLess(abs(avgdiff), 5.2e18)

            if PLOT:
                pylab.figure(2)
                pylab.pcolor(abs(so - soInterpInterp), vmin=0.0, vmax=1.0)
                pylab.colorbar()
                pylab.title('ESMF linear')


    def test_2d_esmf_interface(self):
        #print 'running test_2d_esmf_interface...'
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]
        clt = cdms2.open(sys.prefix + 'sample_data/clt.nc')('clt')[0, :, :]
        tic = time.time()
        # assume so and clt are cell centered
        srcGrid = regrid2.esmf.EsmfStructGrid(so.shape, 
                                              coordSys=ESMP.ESMP_COORDSYS_SPH_DEG,
                                              periodicity=0)
        dstGrid = regrid2.esmf.EsmfStructGrid(clt.shape, 
                                              coordSys=ESMP.ESMP_COORDSYS_SPH_DEG,
                                              periodicity=0)
        srcGrid.setCoords([so.getGrid().getLatitude(), so.getGrid().getLongitude()], 
                          staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        # convert to curvilinear
        ny, nx = clt.shape
        y = clt.getGrid().getLatitude()
        x = clt.getGrid().getLongitude()
        yy = numpy.outer(y, numpy.ones((nx,), numpy.float32))
        xx = numpy.outer(numpy.ones((ny,), numpy.float32), x)
        dstGrid.setCoords([yy, xx], 
                          staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        mask = numpy.zeros(so.shape, numpy.int32)
        mask[:] = (so == so.missing_value)
        srcGrid.setMask(mask)
        srcFld = regrid2.esmf.EsmfStructField(srcGrid, 'srcFld', 
                                              datatype = so.dtype,
                                              staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        srcFld.setLocalData(numpy.array(so), staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        dstFld = regrid2.esmf.EsmfStructField(dstGrid, 'dstFld', 
                                              datatype = so.dtype,
                                              staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        dsFld.setLocalData(so.missing_value*numpy.ones(clt.shape, numpy.float32),
                           staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        srcFld2 = regrid2.esmf.EsmfStructField(srcGrid, 'srcFld2', 
                                               datatype = so.dtype,
                                               staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        srcFld2.setLocalData(so.missing_value*numpy.ones(so.shape, numpy.float32),
                             staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        
        rgrd1 = regrid2.esmf.EsmfRegrid(srcFld, dstFld, 
                                        srcFrac=None, dstFrac=None,
                                        srcMaskValues=numpy.array([1], numpy.int32),
                                        dstMaskValues=numpy.array([1], numpy.int32),
                                        regridMethod=ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                                        unMappedAction=ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        rgrd1(srcFld, dstFld)
        rgrd2 = regrid2.esmf.EsmfRegrid(dstFld, srcFld2, 
                                        srcFrac=None, dstFrac=None,
                                        srcMaskValues=numpy.array([1], numpy.int32),
                                        dstMaskValues=numpy.array([1], numpy.int32),
                                        regridMethod=ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                                        unMappedAction=ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        rgrd2(dstFld, srcFld2)
        soInterp = numpy.reshape(dstFld.getPointer(), clt.shape)
        soInterpInterp = numpy.reshape(srcFld2.getPointer(), so.shape)

        toc = time.time()
        print 'time to interpolate (ESMF interface) forward/backward: ', toc - tic
        ntot = reduce(operator.mul, so.shape)
        avgdiff = numpy.sum(so - soInterpInterp) / float(ntot)
        print 'avgdiff = ', avgdiff
        self.assertLess(abs(avgdiff), 3.0)

        if PLOT:
            pylab.figure(4)
            pylab.subplot(2,2,1)
            pylab.pcolor(so, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title("esmf.py so")
            pylab.subplot(2,2,2)
            pylab.pcolor(soInterp, vmin=20.0, vmax=40.0)
            pylab.title("esmf.py soInterp")
            pylab.colorbar()
            pylab.subplot(2,2,3)
            pylab.pcolor(soInterpInterp, vmin=20.0, vmax=40.0)
            pylab.title("esmf.py soInterpInterp")
            pylab.colorbar()
            pylab.subplot(2,2,4)
            pylab.pcolor(abs(so - soInterpInterp), vmin=-0.5, vmax=0.5)
            pylab.colorbar()
            pylab.title("esmf.py error")


if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


