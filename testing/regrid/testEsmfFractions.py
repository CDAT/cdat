"""
$Id: testEsmfFractions.py 2354 2012-07-11 15:28:14Z pletzer $

Unit test for src/dst AreaFractions return nan. It only fails in tripolar grids.

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
from matplotlib import pylab as pl
from mpi4py import MPI
import sys

PLOT = False

class Test(unittest.TestCase):
    def setUp(self):
        pass

    def test1_2d_esmf_native_tripolar_fraction(self):

        mype = MPI.COMM_WORLD.Get_rank()

        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]

        h = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_HadGEM2-CC_historical_r1i1p1_185912-186911_2timesteps.nc')
        hadGEM2Model = h('so')[0,0,...]

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

        srcCells = [so.getLatitude(), so.getLongitude()]
        srcNodes = [srcLatCorner, srcLonCorner]

        clt = cdms2.open(sys.prefix + 'sample_data/clt.nc')('clt')[0, :, :]
        cltBounds = clt.getGrid().getBounds()

        ny, nx = clt.shape

        # clt grid is rectilinear, transform to curvilinear
        CLGrid = clt.getGrid().toCurveGrid()
        #lats = CLGrid.getLatitude()[:].data
        lons = CLGrid.getLongitude()[:].data
        
        # Make the bounds go from -90, 90 with uniform spacing and the 
        # Cell Centers go from -88.something to 88.something
        yb = numpy.linspace(-90, 90,ny+1)
        interval = abs(yb[0]-yb[1])
        y  = numpy.linspace(-90+interval/2., 90-interval/2., ny)
        lats = numpy.outer(y, numpy.ones((nx), numpy.float32))

        ny, nx = clt.shape
        #yb = numpy.zeros((ny+1,), numpy.float32)
        #yb[:ny] = cltBounds[0][:, 0]
        #yb[ny] = cltBounds[0][ny-1, 1]
        xb = numpy.zeros((nx+1,), numpy.float32)
        xb[:nx] = cltBounds[1][:, 0]
        xb[nx] = cltBounds[1][nx-1, 1]

        # make curvilinear
        dstLatCorner = numpy.outer(yb, numpy.ones( (nx+1,), numpy.float32 ))
        dstLonCorner = numpy.outer(numpy.ones( (ny+1,), numpy.float32 ), xb)

        dstCells = [lats, lons]
        dstNodes = [dstLatCorner, dstLonCorner]

        print 'running test2_2d_esmf_native_tripolar_fraction...'
        tic = time.time()
        # create grid
        srcMaxIndex = numpy.array(so.shape[::-1], dtype=numpy.int32)
        srcGrid = ESMP.ESMP_GridCreate1PeriDim(srcMaxIndex,
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
        dstGrid = ESMP.ESMP_GridCreate1PeriDim(dstMaxIndex,
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

        dstFld = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstFld',
                                        typekind = ESMP.ESMP_TYPEKIND_R4,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFldPtr = ESMP.ESMP_FieldGetPtr(dstFld)

        # Create the field for the fractional areas
        srcFracFld = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFrac',
                                        typekind = ESMP.ESMP_TYPEKIND_R4,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFracPtr = ESMP.ESMP_FieldGetPtr(srcFracFld)
        dstFracFld = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstFrac',
                                        typekind = ESMP.ESMP_TYPEKIND_R4,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFracPtr = ESMP.ESMP_FieldGetPtr(dstFracFld)

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

        srcXCenter[:] = numpy.reshape(srcCells[1][srcJCenterBeg:srcJCenterEnd,
                                                   srcICenterBeg:srcICenterEnd],
                                      (srcNtotCenter,))
        srcYCenter[:] = numpy.reshape(srcCells[0][srcJCenterBeg:srcJCenterEnd,
                                                   srcICenterBeg:srcICenterEnd],
                                      (srcNtotCenter,))
        srcXCorner[:] = numpy.reshape(srcNodes[1][srcJCornerBeg:srcJCornerEnd,
                                                   srcICornerBeg:srcICornerEnd],
                                      (srcNtotCorner,))
        srcYCorner[:] = numpy.reshape(srcNodes[0][srcJCornerBeg:srcJCornerEnd,
                                                   srcICornerBeg:srcICornerEnd],
                                      (srcNtotCorner,))
        srcFldPtr[:] = numpy.reshape(so[srcJCenterBeg:srcJCenterEnd,
                                        srcICenterBeg:srcICenterEnd],
                                     (srcNtotCenter,))
        srcMask[:] = (srcFldPtr == so.missing_value)

        srcFracPtr[:] = -999
        dstFracPtr[:] = -999

        dstNtotCenter = reduce(operator.mul, [dstDimsCenter[1][i] - dstDimsCenter[0][i] for i in range(2)])
        dstNtotCorner = reduce(operator.mul, [dstDimsCorner[1][i] - dstDimsCorner[0][i] for i in range(2)])

        dstXCenter[:] = numpy.reshape(dstCells[1][dstDimsCenter[0][1]:dstDimsCenter[1][1],
                                                       dstDimsCenter[0][0]:dstDimsCenter[1][0]],
                                      (dstNtotCenter))
        dstXCenter[:] = numpy.reshape(dstCells[0][dstDimsCenter[0][1]:dstDimsCenter[1][1],
                                                       dstDimsCenter[0][0]:dstDimsCenter[1][0]],
                                      (dstNtotCenter))
        dstXCorner[:] = numpy.reshape(dstNodes[1][dstDimsCorner[0][1]:dstDimsCorner[1][1],
                                                       dstDimsCorner[0][0]:dstDimsCorner[1][0]],
                                      (dstNtotCorner,))
        dstYCorner[:] = numpy.reshape(dstNodes[0][dstDimsCorner[0][1]:dstDimsCorner[1][1],
                                                       dstDimsCorner[0][0]:dstDimsCorner[1][0]],
                                      (dstNtotCorner,))
        dstFldPtr[:] = 0
        srcAreaFld = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcArea')
        dstAreaFld = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstArea')

        # regrid forward and backward
        maskVals = numpy.array([1], numpy.int32) # values defining mask
        regrid1 = ESMP.ESMP_FieldRegridStore(srcFld,
                                             dstFld,
                                             srcMaskValues=maskVals,
                                             dstMaskValues=None,
                                             regridmethod=ESMP.ESMP_REGRIDMETHOD_CONSERVE,
                                             unmappedaction=ESMP.ESMP_UNMAPPEDACTION_IGNORE,
                                             srcFracField=srcFracFld,
                                             dstFracField=dstFracFld)

        ESMP.ESMP_FieldRegrid(srcFld, dstFld, regrid1)

        srcAreas = ESMP.ESMP_FieldRegridGetArea(srcAreaFld)
        dstAreas = ESMP.ESMP_FieldRegridGetArea(dstAreaFld)

        srcAreaPtr = ESMP.ESMP_FieldGetPtr(srcAreaFld)
        dstAreaPtr = ESMP.ESMP_FieldGetPtr(dstAreaFld)

        if mype == 0:
            srcHasNan = numpy.any(numpy.isnan(srcFracPtr))
            dstHasNan = numpy.any(numpy.isnan(dstFracPtr))

            aa = numpy.isnan(srcFracPtr)
            bb = numpy.isnan(dstFracPtr)

            cc = srcFldPtr == 0
            dd = dstFldPtr == 0
            
            if PLOT:
                pl.figure(1)
                pl.subplot(2,1,1)
                pl.pcolor(numpy.reshape(aa, so.shape))
                pl.colorbar()
                pl.title('source')
                pl.subplot(2,1,2)
                pl.pcolor(numpy.reshape(bb, clt.shape))
                pl.colorbar()
                pl.title('destination')
                pl.suptitle("Red == location of nan's")

            print srcHasNan, dstHasNan

            # Do they have nans?
            self.assertFalse(srcHasNan, True)
            self.assertFalse(dstHasNan, True)

            jbeg, jend = dstDimsCenter[0][1], dstDimsCenter[1][1]
            ibeg, iend = dstDimsCenter[0][0], dstDimsCenter[1][0]
            soInterp = numpy.reshape(dstFldPtr, (jend-jbeg, iend-ibeg))

        toc = time.time()

        # clean up
        ESMP.ESMP_FieldRegridRelease(regrid1)
        ESMP.ESMP_FieldDestroy(dstFld)
        ESMP.ESMP_GridDestroy(dstGrid)
        ESMP.ESMP_FieldDestroy(srcFld)
        ESMP.ESMP_GridDestroy(srcGrid)

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pl.show()


