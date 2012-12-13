"""
$Id: testEsmfInterface.py 2389 2012-07-26 15:51:43Z dkindig $

Unit tests for esmf interface

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
from mpi4py import MPI
import sys

PLOT = False

class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test_2d(self):

        mype = MPI.COMM_WORLD.Get_rank()

        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f['so'] #[0, 0, :, :]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, :, :]

        # ESMF interface, assume so and clt are cell centered
        srcGrid = regrid2.esmf.EsmfStructGrid(so[0, 0, ...].shape, 
                                              coordSys=ESMP.ESMP_COORDSYS_SPH_DEG,
                                              periodicity=0)
        dstGrid = regrid2.esmf.EsmfStructGrid(clt.shape, 
                                              coordSys=ESMP.ESMP_COORDSYS_SPH_DEG,
                                              periodicity=0)
        grid = [so.getGrid().getLatitude(), so.getGrid().getLongitude()]
        srcGrid.setCoords([numpy.array(g[:]) for g in grid], 
                          staggerloc=ESMP.ESMP_STAGGERLOC_CENTER,
                          globalIndexing = True)
        # convert to curvilinear
        ny, nx = clt.shape
        y = clt.getGrid().getLatitude()
        x = clt.getGrid().getLongitude()
        yy = numpy.outer(y, numpy.ones((nx,), numpy.float32))
        xx = numpy.outer(numpy.ones((ny,), numpy.float32), x)
        dstGrid.setCoords([yy, xx], 
                          staggerloc=ESMP.ESMP_STAGGERLOC_CENTER,
                          globalIndexing = True)
        mask = numpy.zeros(so[0, 0, ...].shape, numpy.int32)
        mask[:] = (so[0, 0, ...] == so.missing_value)
        srcGrid.setMask(mask)
        srcFld = regrid2.esmf.EsmfStructField(srcGrid, 'srcFld', datatype = so[:].dtype,
                                              staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcSlab = srcGrid.getLocalSlab(ESMP.ESMP_STAGGERLOC_CENTER)
        dstSlab = dstGrid.getLocalSlab(ESMP.ESMP_STAGGERLOC_CENTER)
        srcFld.setLocalData(numpy.array(so[0,0,srcSlab[0], srcSlab[1]]), 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFld = regrid2.esmf.EsmfStructField(dstGrid, 'dstFld', 
                                              datatype = so.dtype,
                                              staggerloc=ESMP.ESMP_STAGGERLOC_CENTER)
        dstData = numpy.ones(clt.shape, numpy.float32)[dstSlab[0], dstSlab[1]]
        dstFld.setLocalData(so.missing_value*dstData,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        
        rgrd1 = regrid2.esmf.EsmfRegrid(srcFld, dstFld, 
                                        srcFrac=None, dstFrac=None,
                                        srcMaskValues=numpy.array([1], numpy.int32),
                                        dstMaskValues=numpy.array([1], numpy.int32),
                                        regridMethod=ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                                        unMappedAction=ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        
        # now interpolate 
        rgrd1(srcFld, dstFld)

        # get the data on this proc
        soInterpEsmfInterface = dstFld.getData(rootPe = None)

        # gather the data on proc 0
        soInterpEsmfInterfaceRoot = dstFld.getData(rootPe = 0)

        print '[%d] esmfInterface chksum = %f' % (mype, soInterpEsmfInterface.sum())
        if mype == 0:
             print 'ROOT esmfInterface chksum = %f' % soInterpEsmfInterfaceRoot.sum()

        # Native ESMP
        srcMaxIndex = numpy.array(so[0, 0, ...].shape[::-1], dtype=numpy.int32)
        srcGrid = ESMP.ESMP_GridCreateNoPeriDim(srcMaxIndex, 
                                                coordSys = ESMP.ESMP_COORDSYS_SPH_DEG)
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
        srcFldPtr[:] = numpy.reshape(so[0, 0, srcDimsCenter[0][1]:srcDimsCenter[1][1],
                                        srcDimsCenter[0][0]:srcDimsCenter[1][0]], 
                                     (srcNtot,))
        srcMask[:] = (srcFldPtr == so.missing_value)

        dstNtot = reduce(operator.mul, [dstDimsCenter[1][i] - dstDimsCenter[0][i] for i in range(2)])

        # clt grid is rectilinear, transform to curvilinear
        lons = clt.getGrid().getLongitude()
        lats = clt.getGrid().getLatitude()
        ny, nx = dstDimsCenter[1][1]-dstDimsCenter[0][1], dstDimsCenter[1][0]-dstDimsCenter[0][0]
        localLons = lons[dstDimsCenter[0][0]:dstDimsCenter[1][0]]
        localLats = lats[dstDimsCenter[0][1]:dstDimsCenter[1][1]]
        xx = numpy.outer(numpy.ones((ny,), dtype=numpy.float32), localLons)
        yy = numpy.outer(localLats, numpy.ones((nx,), dtype=numpy.float32))

        dstXCenter[:] = xx.reshape( (dstNtot,) )
        dstYCenter[:] = yy.reshape( (dstNtot,) )
        dstFldPtr[:] = so.missing_value

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

        ESMP.ESMP_FieldRegrid(srcFld, dstFld, regrid1, zeroregion=ESMP.ESMP_REGION_SELECT)

        jbeg, jend = dstDimsCenter[0][1], dstDimsCenter[1][1]
        ibeg, iend = dstDimsCenter[0][0], dstDimsCenter[1][0]        
        soInterpESMP = numpy.reshape(dstFldPtr, (jend-jbeg, iend-ibeg))

        # check local diffs
        ntot = reduce(operator.mul, soInterpESMP.shape)
        avgdiff = numpy.sum(soInterpEsmfInterface - soInterpESMP) / float(ntot)
        self.assertLess(abs(avgdiff), 1.e-7)

        # check gather
        chksumESMP = numpy.sum(soInterpESMP)
        chksumEsmfInterface = numpy.sum(soInterpEsmfInterface)
        chksumsESMP = MPI.COMM_WORLD.gather(chksumESMP, root=0)

        print '[%d] ESMP chksum = %f' % (mype, chksumESMP)
        if mype == 0:
            print 'ROOT ESMP chksum = %f' % numpy.sum(chksumsESMP)

        if mype == 0:
            chksumESMPRoot = numpy.sum(chksumsESMP)
            chksumESMFInterfaceRoot = numpy.sum(soInterpEsmfInterfaceRoot)
            self.assertLess(abs(chksumESMFInterfaceRoot - chksumESMPRoot), 1.e-5*chksumESMPRoot)
                
        if PLOT:
            pylab.subplot(2, 2, 1)
            pylab.pcolor(so, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('so')
            pylab.subplot(2, 2, 2)
            pylab.pcolor(soInterpEsmfInterface - soInterpESMP, vmin=-0.5, vmax=0.5)
            pylab.colorbar()
            pylab.title('[%d] EsmfInterface - ESMP' % mype)
            pylab.subplot(2, 2, 3)
            pylab.pcolor(soInterpEsmfInterface, vmin=20.0, vmax=40.0)
            pylab.colorbar()
            pylab.title('[%d] ESMFInterface' % mype)
            pylab.subplot(2, 2, 4)
            pylab.pcolor(soInterpESMP, vmin=20, vmax=40)
            pylab.colorbar()
            pylab.title('[%d] ESMP' % mype)

        # clean up
        ESMP.ESMP_FieldRegridRelease(regrid1)
        ESMP.ESMP_FieldDestroy(dstFld)
        ESMP.ESMP_GridDestroy(dstGrid)
        ESMP.ESMP_FieldDestroy(srcFld)
        ESMP.ESMP_GridDestroy(srcGrid)       

if __name__ == '__main__':
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


