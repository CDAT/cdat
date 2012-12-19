"""
$Id: testConserv.py 2354 2012-07-11 15:28:14Z pletzer $

Unit tests for conservative interpolation

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

    def test_2d_esmf_native(self):
        print 'running test_2d_esmf_native...'
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]

        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, :, :]
        cltBounds = clt.getGrid().getBounds()

        tic = time.time()

        ny, nx = clt.shape
        yb = numpy.zeros((ny+1,), numpy.float32)
        yb[:ny] = cltBounds[0][:, 0]
        yb[ny] = cltBounds[0][ny-1, 1]
        xb = numpy.zeros((nx+1,), numpy.float32)
        xb[:nx] = cltBounds[1][:, 0]
        xb[nx] = cltBounds[1][nx-1, 1] 
        # make curvilinear
        dstLatCorner = numpy.outer(yb, numpy.ones( (nx+1,), numpy.float32 ))
        dstLonCorner = numpy.outer(numpy.ones( (ny+1,), numpy.float32 ), xb)

        ny, nx = so.shape
        soBounds = so.getGrid().getBounds()

        srcLatCorner = numpy.zeros( (ny+1, nx+1), numpy.float32 )
        srcLatCorner[:ny, :nx] = soBounds[0][:, :, 0]
        srcLatCorner[:ny, nx] = soBounds[0][:ny, nx-1, 1]
        srcLatCorner[ny, nx] = soBounds[0][ny-1, nx-1, 2]
        srcLatCorner[ny, :nx] = soBounds[0][ny-1, :nx, 3]

        srcLonCorner = numpy.zeros( (ny+1, nx+1), numpy.float32 )
        srcLonCorner[:ny, :nx] = soBounds[1][:, :, 0]
        srcLonCorner[:ny, nx] = soBounds[1][:ny, nx-1, 1]
        srcLonCorner[ny, nx] = soBounds[1][ny-1, nx-1, 2]
        srcLonCorner[ny, :nx] = soBounds[1][ny-1, :nx, 3]

        # create grid
        srcMaxIndex = numpy.array(so.shape[::-1], dtype=numpy.int32)
        srcGrid = ESMP.ESMP_GridCreateNoPeriDim(srcMaxIndex, 
                                                coordSys = ESMP.ESMP_COORDSYS_SPH_DEG)
        ESMP.ESMP_GridAddCoord(srcGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        ESMP.ESMP_GridAddCoord(srcGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
        srcDimsCenter = ESMP.ESMP_GridGetCoord(srcGrid,
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        srcDimsCorner = ESMP.ESMP_GridGetCoord(srcGrid,
                                               ESMP.ESMP_STAGGERLOC_CORNER)
        srcXCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        srcYCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        srcXCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CORNER)
        srcYCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CORNER)

        dstMaxIndex = numpy.array(clt.shape[::-1], dtype=numpy.int32)
        dstGrid = ESMP.ESMP_GridCreateNoPeriDim(dstMaxIndex, 
                                                coordSys = ESMP.ESMP_COORDSYS_SPH_DEG)
        ESMP.ESMP_GridAddCoord(dstGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        ESMP.ESMP_GridAddCoord(dstGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
        dstDimsCenter = ESMP.ESMP_GridGetCoord(dstGrid,
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        dstDimsCorner = ESMP.ESMP_GridGetCoord(dstGrid,
                                               ESMP.ESMP_STAGGERLOC_CORNER)
        dstXCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        dstYCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        dstXCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CORNER)
        dstYCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CORNER)
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
        
        srcNtotCenter = reduce(operator.mul, 
                               [srcDimsCenter[1][i] - srcDimsCenter[0][i] \
                                    for i in range(2)])
        srcNtotCorner = reduce(operator.mul, 
                               [srcDimsCorner[1][i] - srcDimsCorner[0][i] \
                                    for i in range(2)])
        srcJCenterBeg = srcDimsCenter[0][1]
        srcJCenterEnd = srcDimsCenter[1][1]
        srcJCornerBeg = srcDimsCorner[0][1]
        srcJCornerEnd = srcDimsCorner[1][1]
        srcICenterBeg = srcDimsCenter[0][0]
        srcICenterEnd = srcDimsCenter[1][0]
        srcICornerBeg = srcDimsCorner[0][0]
        srcICornerEnd = srcDimsCorner[1][0]
        srcLonCenter = so.getLongitude()
        srcLatCenter = so.getLatitude()
        srcXCenter[:] = numpy.reshape(srcLonCenter[srcJCenterBeg:srcJCenterEnd,
                                                   srcICenterBeg:srcICenterEnd], 
                                      (srcNtotCenter,))
        srcYCenter[:] = numpy.reshape(srcLatCenter[srcJCenterBeg:srcJCenterEnd,
                                                   srcICenterBeg:srcICenterEnd], 
                                      (srcNtotCenter,))
        srcXCorner[:] = numpy.reshape(srcLonCorner[srcJCornerBeg:srcJCornerEnd, 
                                                   srcICornerBeg:srcICornerEnd], 
                                      (srcNtotCorner,))
        srcYCorner[:] = numpy.reshape(srcLatCorner[srcJCornerBeg:srcJCornerEnd, 
                                                   srcICornerBeg:srcICornerEnd], 
                                      (srcNtotCorner,))
        srcFldPtr[:] = numpy.reshape(so[srcJCenterBeg:srcJCenterEnd,
                                        srcICenterBeg:srcICenterEnd], 
                                     (srcNtotCenter,))
        srcMask[:] = (srcFldPtr == so.missing_value)
        

        dstNtotCenter = reduce(operator.mul, [dstDimsCenter[1][i] - dstDimsCenter[0][i] for i in range(2)])
        dstNtotCorner = reduce(operator.mul, [dstDimsCorner[1][i] - dstDimsCorner[0][i] for i in range(2)])
        

        # clt grid is rectilinear, transform to curvilinear
        lons = clt.getGrid().getLongitude()
        lats = clt.getGrid().getLatitude()
        ny, nx = dstDimsCenter[1][1]-dstDimsCenter[0][1], dstDimsCenter[1][0]-dstDimsCenter[0][0]
        yy = numpy.outer(lats[dstDimsCenter[0][1]:dstDimsCenter[1][1]], numpy.ones((nx,), dtype=numpy.float32))
        xx = numpy.outer(numpy.ones((ny,), dtype=numpy.float32), lons[dstDimsCenter[0][0]:dstDimsCenter[1][0]])
        y = yy.reshape( (dstNtotCenter,) )
        x = xx.reshape( (dstNtotCenter,) )

        dstXCenter[:] = x[:]
        dstYCenter[:] = y[:]
        dstXCorner[:] = numpy.reshape(dstLonCorner[dstDimsCorner[0][1]:dstDimsCorner[1][1],
                                                   dstDimsCorner[0][0]:dstDimsCorner[1][0]], 
                                      (dstNtotCorner,))
        dstYCorner[:] = numpy.reshape(dstLatCorner[dstDimsCorner[0][1]:dstDimsCorner[1][1],
                                                   dstDimsCorner[0][0]:dstDimsCorner[1][0]], 
                                      (dstNtotCorner,))
        dstFldPtr[:] = 0

        # regrid forward and backward
        maskVals = numpy.array([1], numpy.int32) # values defining mask
        regrid1 = ESMP.ESMP_FieldRegridStore(srcFld, 
                                             dstFld, 
                                             srcMaskValues=maskVals, 
                                             dstMaskValues=None, 
                                             regridmethod=ESMP.ESMP_REGRIDMETHOD_CONSERVE, 
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
                                             regridmethod=ESMP.ESMP_REGRIDMETHOD_CONSERVE, 
                                             unmappedaction=ESMP.ESMP_UNMAPPEDACTION_IGNORE, 
                                             srcFracField=None, 
                                             dstFracField=None)
        ESMP.ESMP_FieldRegrid(dstFld, srcFld2, regrid2)

        jbeg, jend = srcDimsCenter[0][1], srcDimsCenter[1][1]
        ibeg, iend = srcDimsCenter[0][0], srcDimsCenter[1][0]
        soInterpInterp = numpy.reshape(srcFldPtr2, (jend-jbeg, iend-ibeg))
        
        toc = time.time()
        avgdiff = numpy.sum(so[jbeg:jend,ibeg:iend] - soInterpInterp) / float(srcNtotCenter)
        print 'avgdiff = ', avgdiff
        self.assertLess(abs(avgdiff), 3.0)
        
        if PLOT:
            pylab.figure(1)
            pylab.subplot(2, 2, 1)
            pylab.pcolor(so, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF conserve native: so')
            pylab.subplot(2, 2, 2)
            pylab.pcolor(soInterp, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF conserve native: soInterp')
            pylab.subplot(2, 2, 3)
            pylab.pcolor(soInterpInterp, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF conserve native: soInterpInterp')
            pylab.subplot(2, 2, 4)
            pylab.pcolor(so[jbeg:jend,ibeg:iend] - soInterpInterp, vmin=-0.5, vmax=0.5)
            pylab.colorbar()
            pylab.title('ESMF conserve native: error')

        # clean up
        ESMP.ESMP_FieldRegridRelease(regrid2)
        ESMP.ESMP_FieldRegridRelease(regrid1)
        ESMP.ESMP_FieldDestroy(dstFld)
        ESMP.ESMP_GridDestroy(dstGrid)
        ESMP.ESMP_FieldDestroy(srcFld)
        ESMP.ESMP_FieldDestroy(srcFld2)
        ESMP.ESMP_GridDestroy(srcGrid)
        

    def test_2d_esmf_conserv(self):
        print 'running test_2d_esmf_conserv...'
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, :, :]
        tic = time.time()
        soInterp = so.regrid(clt.getGrid(), regridTool='ESMF', regridMethod='Conservative')
        soInterpInterp = soInterp.regrid(so.getGrid(), regridTool='ESMF', 
                                         regridMethod='Conservative')
        toc = time.time()
        print 'time to interpolate (ESMF conservative) forward/backward: ', toc - tic
        ntot = reduce(operator.mul, so.shape)
        avgdiff = numpy.sum(so - soInterpInterp) / float(ntot)
        print 'avgdiff = ', avgdiff

        if PLOT:
            pylab.figure(2)
            pylab.subplot(2, 2, 1)
            pylab.pcolor(so, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF conserve regrid: so')
            pylab.subplot(2, 2, 2)
            pylab.pcolor(soInterp, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF conserve regrid: soInterp')
            pylab.subplot(2, 2, 3)
            pylab.pcolor(soInterpInterp, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('ESMF conserve regrid: soInterpInterp')
            pylab.subplot(2, 2, 4)
            pylab.pcolor(so - soInterpInterp, vmin=-0.5, vmax=0.5)
            pylab.colorbar()
            pylab.title('ESMF conserve regrid: error')

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


