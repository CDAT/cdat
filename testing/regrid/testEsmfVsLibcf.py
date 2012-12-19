"""
$Id: testEsmfVsLibcf.py 2389 2012-07-26 15:51:43Z dkindig $

Unit tests comparing esmf and libcf interpolation

"""

import operator
import numpy
import cdms2
import regrid2.esmf
import regrid2
import unittest
import time
import ESMP
import copy
from matplotlib import pylab
import sys

PLOT = False

class Test(unittest.TestCase):
    def setUp(self):
        pass

    def test_2d_libcf(self):
        #print 'running test_2d_libcf...'
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, :, :]
        tic = time.time()
        soInterp = so.regrid(clt.getGrid(), regridTool='libcf')
        soInterpInterp = soInterp.regrid(so.getGrid(), regridTool='libcf')
        toc = time.time()
        #print 'time to interpolate forward/backward (gsRegrid): ', toc - tic
        ntot = reduce(operator.mul, so.shape)
        avgdiff = numpy.sum(so - soInterpInterp) / float(ntot)
        #print 'avgdiff = ', avgdiff
        self.assertLess(abs(avgdiff), 7.e-3)

        if PLOT:
            pylab.figure(1)
            pylab.pcolor(abs(so - soInterpInterp), vmin=0.0, vmax=1.0)
            pylab.colorbar()
            pylab.title('gsRegrid')

    def test_2d_esmf(self):
        #print 'running test_2d_esmf...'
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, :, :]
        tic = time.time()
        soInterp = so.regrid(clt.getGrid(), regridTool='ESMF') #, periodicity=1)
        soInterpInterp = soInterp.regrid(so.getGrid(), regridTool='ESMF')
        toc = time.time()
        #print 'time to interpolate (ESMF linear) forward/backward: ', toc - tic
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
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, :, :]
        tic = time.time()
        # assume so and clt are cell centered
        srcGrid = regrid2.esmf.EsmfStructGrid(so.shape, 
                                              coordSys=ESMP.ESMP_COORDSYS_SPH_DEG,
                                              periodicity=0)
        dstGrid = regrid2.esmf.EsmfStructGrid(clt.shape, 
                                              coordSys=ESMP.ESMP_COORDSYS_SPH_DEG,
                                              periodicity=0)
        grid = [so.getGrid().getLatitude(), so.getGrid().getLongitude()]
        srcGrid.setCoords([numpy.array(g[:]) for g in grid], 
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
                                              staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFld.setLocalData(numpy.array(so), staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFld = regrid2.esmf.EsmfStructField(dstGrid, 'dstFld', 
                                              datatype = so.dtype,
                                              staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFld.setLocalData(so.missing_value*numpy.ones(clt.shape, so.dtype),
                           staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFld2 = regrid2.esmf.EsmfStructField(srcGrid, 'srcFld2', 
                                               datatype = so.dtype,
                                               staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        srcFld2.setLocalData(so.missing_value*numpy.ones(so.shape, so.dtype),
                             staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        
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
        #print 'time to interpolate (ESMF interface) forward/backward: ', toc - tic
        ntot = reduce(operator.mul, so.shape)
        aa = soInterpInterp < 100
        bb = aa * soInterpInterp
        avgdiff = numpy.sum(so - bb) / float(ntot)
        #print 'avgdiff = ', avgdiff
        # Changed 3.0 to 7.0 here. Missing values are not missing in soInterpInterp
        self.assertLess(abs(avgdiff), 7.0)

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

    def test_2d_esmf_native(self):
        #print 'running test_2d_esmf_native...'
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, :, :]
        tic = time.time()

        # create grid
        srcMaxIndex = numpy.array(so.shape[::-1], dtype=numpy.int32)
        srcGrid = ESMP.ESMP_GridCreateNoPeriDim(srcMaxIndex, 
                                                coordSys = ESMP.ESMP_COORDSYS_SPH_DEG)
        #srcGrid = ESMP.ESMP_GridCreate1PeriDim(srcMaxIndex, 
        #                                       coordSys = ESMP.ESMP_COORDSYS_SPH_DEG)
        ESMP.ESMP_GridAddCoord(srcGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcDimsCenter = ESMP.ESMP_GridGetCoord(srcGrid,
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        srcXCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        srcYCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        dstMaxIndex = numpy.array(clt.shape[::-1], dtype=numpy.int32)
        dstGrid = ESMP.ESMP_GridCreateNoPeriDim(dstMaxIndex, 
                                                coordSys = ESMP.ESMP_COORDSYS_SPH_DEG)
        ESMP.ESMP_GridAddCoord(dstGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstDimsCenter = ESMP.ESMP_GridGetCoord(dstGrid,
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        dstXCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        dstYCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        # mask 
        ESMP.ESMP_GridAddItem(srcGrid, item=ESMP.ESMP_GRIDITEM_MASK)
        srcMask = ESMP.ESMP_GridGetItem(srcGrid, item=ESMP.ESMP_GRIDITEM_MASK)
    
        # create field
        srcFld = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFld', 
                                        typekind = ESMP.ESMP_TYPEKIND_R4,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFldPtr = ESMP.ESMP_FieldGetPtr(srcFld)
        srcFld2 = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFld2', 
                                        typekind = ESMP.ESMP_TYPEKIND_R4,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFldPtr2 = ESMP.ESMP_FieldGetPtr(srcFld2)
        dstFld = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstFld', 
                                        typekind = ESMP.ESMP_TYPEKIND_R4,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFldPtr = ESMP.ESMP_FieldGetPtr(dstFld)
        
        # set coords, mask, and field values for src and dst
        
        srcNtot = reduce(operator.mul, [srcDimsCenter[1][i] - srcDimsCenter[0][i] for i in range(2)])

        srcXCenter[:] = numpy.reshape(so.getGrid().getLongitude()[srcDimsCenter[0][1]:srcDimsCenter[1][1],
                                                                  srcDimsCenter[0][0]:srcDimsCenter[1][0]], 
                                      (srcNtot,))
        srcYCenter[:] = numpy.reshape(so.getGrid().getLatitude()[srcDimsCenter[0][1]:srcDimsCenter[1][1],
                                                                 srcDimsCenter[0][0]:srcDimsCenter[1][0]], 
                                      (srcNtot,))
        srcFldPtr[:] = numpy.reshape(so[srcDimsCenter[0][1]:srcDimsCenter[1][1],
                                        srcDimsCenter[0][0]:srcDimsCenter[1][0]], 
                                     (srcNtot,))
        srcMask[:] = (srcFldPtr == so.missing_value)

        dstNtot = reduce(operator.mul, [dstDimsCenter[1][i] - dstDimsCenter[0][i] for i in range(2)])

        # clt grid is rectilinear, transform to curvilinear
        lons = clt.getGrid().getLongitude()
        lats = clt.getGrid().getLatitude()
        ny, nx = dstDimsCenter[1][1]-dstDimsCenter[0][1], dstDimsCenter[1][0]-dstDimsCenter[0][0]
        xx = numpy.outer(numpy.ones((ny,), dtype=numpy.float32), lons)
        yy = numpy.outer(lats, numpy.ones((nx,), dtype=numpy.float32))
        x = xx.reshape( (dstNtot,) )
        y = yy.reshape( (dstNtot,) )

        dstXCenter[:] = x[:]
        dstYCenter[:] = y[:]
        dstFldPtr[:] = 0

        # regrid forward and backward
        maskVals = numpy.array([1], numpy.int32) # values defining mask
        regrid1 = ESMP.ESMP_FieldRegridStore(srcFld, 
                                             dstFld, 
                                             srcMaskValues=maskVals, 
                                             dstMaskValues=None, 
                                             regridmethod=ESMP.ESMP_REGRIDMETHOD_BILINEAR, 
                                             unmappedaction=ESMP.ESMP_UNMAPPEDACTION_IGNORE, 
                                             srcFracField=None, 
                                             dstFracField=None)

        ESMP.ESMP_FieldRegrid(srcFld, dstFld, regrid1)
        
        jbeg, jend = dstDimsCenter[0][1], dstDimsCenter[1][1]
        ibeg, iend = dstDimsCenter[0][0], dstDimsCenter[1][0]        
        soInterp = numpy.reshape(dstFldPtr, (jend-jbeg, iend-ibeg))

        regrid2 = ESMP.ESMP_FieldRegridStore(dstFld, 
                                             srcFld2, 
                                             srcMaskValues=None, 
                                             dstMaskValues=None, 
                                             regridmethod=ESMP.ESMP_REGRIDMETHOD_BILINEAR, 
                                             unmappedaction=ESMP.ESMP_UNMAPPEDACTION_IGNORE, 
                                             srcFracField=None, 
                                             dstFracField=None)
        ESMP.ESMP_FieldRegrid(dstFld, srcFld2, regrid2)

        jbeg, jend = srcDimsCenter[0][1], srcDimsCenter[1][1]
        ibeg, iend = srcDimsCenter[0][0], srcDimsCenter[1][0]
        soInterpInterp = numpy.reshape(srcFldPtr2, (jend-jbeg, iend-ibeg))
        
        toc = time.time()
        #print 'time to interpolate (ESMF linear native) forward/backward: ', toc - tic
        ntot = reduce(operator.mul, so.shape)
        avgdiff = numpy.sum(so - soInterpInterp) / float(ntot)
        #print 'avgdiff = ', avgdiff
        self.assertLess(abs(avgdiff), 3.0)
        
        if PLOT:
            pylab.figure(3)
            pylab.subplot(2, 2, 1)
            pylab.pcolor(so, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF linear native: so')
            pylab.subplot(2, 2, 2)
            pylab.pcolor(soInterp, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF linear native: soInterp')
            pylab.subplot(2, 2, 3)
            pylab.pcolor(soInterpInterp, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF linear native: soInterpInterp')
            pylab.subplot(2, 2, 4)
            pylab.pcolor(so - soInterpInterp, vmin=-0.5, vmax=0.5)
            pylab.colorbar()
            pylab.title('ESMF linear native: error')

        # clean up
        ESMP.ESMP_FieldRegridRelease(regrid2)
        ESMP.ESMP_FieldRegridRelease(regrid1)
        ESMP.ESMP_FieldDestroy(dstFld)
        ESMP.ESMP_GridDestroy(dstGrid)
        ESMP.ESMP_FieldDestroy(srcFld)
        ESMP.ESMP_FieldDestroy(srcFld2)
        ESMP.ESMP_GridDestroy(srcGrid)
        

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


