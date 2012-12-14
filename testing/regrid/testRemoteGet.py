"""
$Id: testRemoteGet.py 2354 2012-07-11 15:28:14Z pletzer $

Unit tests for testing mpi gather onto to root proc

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
import sys

class Test(unittest.TestCase):
    def setUp(self):
        pass

    def test_2d_esmf_native(self):

        print 'running test_2d_esmf_native...'
        f = cdms2.open(sys.prefix + \
                           '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, 0, :, :]

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

        # mask 
        ESMP.ESMP_GridAddItem(srcGrid, item=ESMP.ESMP_GRIDITEM_MASK)
        srcMask = ESMP.ESMP_GridGetItem(srcGrid, item=ESMP.ESMP_GRIDITEM_MASK)
    
        # create field
        srcFld = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFld', 
                                        typekind = ESMP.ESMP_TYPEKIND_R4,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)

        srcFldPtr = ESMP.ESMP_FieldGetPtr(srcFld) # local field

        
        # set coords, mask, and field values for src
        
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
        
        
        # pe local start/end indices
        jbeg, jend = srcDimsCenter[0][1], srcDimsCenter[1][1]
        ibeg, iend = srcDimsCenter[0][0], srcDimsCenter[1][0]

        lo, hi = ESMP.ESMP_GridGetCoord(srcGrid)
        ptr =  ESMP.ESMP_FieldGetPtr(srcFld)
        chksum = numpy.sum(ptr)

        # gather the result on proc 0
        from mpi4py import MPI
        comm = MPI.COMM_WORLD
        chksums = comm.gather(chksum, root = 0)
        ptrs = comm.gather(ptr, root = 0)
        mype = comm.Get_rank()
        nprocs = comm.Get_size()
        if mype == 0:
            for i in range(nprocs):
                chksum = numpy.sum(ptrs[i])
                print '[%d] ptrs[i].shape = %s sum(ptrs[i]) = %f' % (i, 
                                                                     str(ptrs[i].shape), 
                                                                     chksum)
                self.assertEqual(chksum, chksums[i])
        # clean up
        ESMP.ESMP_FieldDestroy(srcFld)
        ESMP.ESMP_GridDestroy(srcGrid)
        
if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)


