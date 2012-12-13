import cdms2
import ESMP
import numpy
import unittest
from mpi4py import MPI

class TestEsmpSmall(unittest.TestCase):

    """
    Test pure ESMP regridding
    """

    def xFunct(self, j, i):
        dx = 1.0/float(self.dims[0])
        return 0.0 + i*dx
    
    def yFunct(self, j, i):
        dy = 1.0/float(self.dims[1])
        return 0.0 + j*dy

    def func(self, x, y):
        if x == 0 and y == 0:
            return 1
        elif x == 0 and y == 1:
            return 1
        elif x == 1 and y == 0:
            return 1
        elif x == 1 and y == 1:
            return 1
        else:
            return 0
        #return x * y

    def setUp(self):
        pass

    def test1_ESMP(self):

        rk = MPI.COMM_WORLD.Get_rank()
        
        coordSys = ESMP.ESMP_COORDSYS_CART
        
        nres = 4
        self.srcDims = (4*nres, 3*nres)
        self.dstDims = (4*nres, 3*nres)

        srcMaxIndex = numpy.array(self.srcDims, numpy.int32) # number of cells
        dstMaxIndex = numpy.array(self.dstDims, numpy.int32) # number of cells

        # grids
        srcGrid = ESMP.ESMP_GridCreateNoPeriDim(srcMaxIndex, 
                                        coordSys = coordSys)
        dstGrid = ESMP.ESMP_GridCreateNoPeriDim(dstMaxIndex, 
                                        coordSys = coordSys)
        ESMP.ESMP_GridAddCoord(srcGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
        ESMP.ESMP_GridAddCoord(srcGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        ESMP.ESMP_GridAddCoord(dstGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
        ESMP.ESMP_GridAddCoord(dstGrid, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)

        # masks
        ESMP.ESMP_GridAddItem(srcGrid, item=ESMP.ESMP_GRIDITEM_MASK)
        srcGridMaskPtr = ESMP.ESMP_GridGetItem(srcGrid, item=ESMP.ESMP_GRIDITEM_MASK)

        ESMP.ESMP_GridAddItem(dstGrid, item=ESMP.ESMP_GRIDITEM_MASK)
        dstGridMaskPtr = ESMP.ESMP_GridGetItem(dstGrid, item=ESMP.ESMP_GRIDITEM_MASK)


        # coordinates
        srcXCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CORNER)
        srcYCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CORNER)
        srcLoCorner, srcHiCorner = ESMP.ESMP_GridGetCoord(srcGrid, ESMP.ESMP_STAGGERLOC_CORNER)

        srcXCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        srcYCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        srcLoCenter, srcHiCenter = ESMP.ESMP_GridGetCoord(srcGrid, ESMP.ESMP_STAGGERLOC_CENTER)


        dstXCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CORNER)
        dstYCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CORNER)
        dstLoCorner, dstHiCorner = ESMP.ESMP_GridGetCoord(dstGrid, ESMP.ESMP_STAGGERLOC_CORNER)
        
        dstXCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid, 0, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        dstYCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid, 1, 
                                               ESMP.ESMP_STAGGERLOC_CENTER)
        dstLoCenter, dstHiCenter = ESMP.ESMP_GridGetCoord(dstGrid, ESMP.ESMP_STAGGERLOC_CENTER)

        # fields
        srcFld = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFld', 
                                        typekind = ESMP.ESMP_TYPEKIND_R8,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFld = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstFld', 
                                        typekind = ESMP.ESMP_TYPEKIND_R8,
                                        staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFieldPtr = ESMP.ESMP_FieldGetPtr(srcFld)
        dstFieldPtr = ESMP.ESMP_FieldGetPtr(dstFld)

        # set the coordinates and field values

        # src-corners
        srcNx1 = srcHiCorner[0] - srcLoCorner[0]
        srcNy1 = self.srcDims[1]+1
        self.dims = self.srcDims
        for iy in range(srcNy1):
            iyp = iy + srcLoCorner[0]
            for ix in range(srcNx1):
                ixp = ix + srcLoCorner[1]
                srcXCorner[ix + iy*srcNx1] = self.xFunct(iyp, ixp)
                srcYCorner[ix + iy*srcNx1] = self.yFunct(iyp, ixp)

        # src-centers and mask 
        srcNx = srcHiCenter[0] - srcLoCenter[0]
        srcNy = srcHiCenter[1] - srcLoCenter[1]
        self.dims = self.srcDims
        for iy in range(srcNy):
            iyp = iy + srcLoCenter[0]
            for ix in range(srcNx):
                ixp = ix + srcLoCenter[1]
                x = self.xFunct(iyp + 0.5, ixp + 0.5)
                y = self.yFunct(iyp + 0.5, ixp + 0.5)
                srcXCenter[ix + iy*srcNx] = x
                srcYCenter[ix + iy*srcNx] = y
                mask = 0
                if ((iyp*ixp) % 3) == 1: 
                    mask = 1
                srcGridMaskPtr[ix + iy*srcNx] = mask # valid
                srcFieldPtr[ix + iy*srcNx] = self.func(ixp, iyp)
        
        # dst-corners
        dstNx1 = dstHiCorner[0] - dstLoCorner[0]
        dstNy1 = dstHiCorner[1] - dstLoCorner[1]
        self.dims = self.dstDims
        for iy in range(dstNy1):
            iyp = iy + dstLoCorner[0]
            for ix in range(dstNx1):
                ixp = ix + dstLoCorner[1]
                dstXCorner[ix + iy*dstNx1] = self.xFunct(iyp, ixp)
                dstYCorner[ix + iy*dstNx1] = self.yFunct(iyp, ixp)

        # dst-centers and mask 
        dstNx = dstHiCenter[0] - dstLoCenter[0]
        dstNy = dstHiCenter[1] - dstLoCenter[1]
        self.dims = self.dstDims
        for iy in range(dstNy):
            iyp = iy + dstLoCenter[0]
            for ix in range(dstNx):
                ixp = ix + dstLoCenter[1]
                x = self.xFunct(iyp + 0.5, ixp + 0.5)
                y = self.yFunct(iyp + 0.5, ixp + 0.5)
                dstXCenter[ix + iy*dstNx] = x
                dstYCenter[ix + iy*dstNx] = y
                dstGridMaskPtr[ix + iy*dstNx] = 0 # valid
                dstFieldPtr[ix + iy*dstNx] = -20 # fill value

        srcAreaField = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcArea')
        dstAreaField = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstArea')
        srcFracField = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFrac')
        dstFracField = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstFrac')

        # IF you want to set your own area. These lines are required. 
        # Otherwise, let ESMF do it.
#        ESMP.ESMP_GridAddItem(srcGrid, item = ESMP.ESMP_GRIDITEM_AREA)
#        srcAreas = ESMP.ESMP_GridGetItem(srcGrid, item = ESMP.ESMP_GRIDITEM_AREA)
#        ESMP.ESMP_GridAddItem(dstGrid, item = ESMP.ESMP_GRIDITEM_AREA)
#        dstAreas = ESMP.ESMP_GridGetItem(dstGrid, item = ESMP.ESMP_GRIDITEM_AREA)
#        srcAreas[:] = 0.02080333333
#        dstAreas[:] = 0.08333333333
        
        # interpolation
        maskVals = numpy.array([1], numpy.int32) # values defining mask
        regrid = ESMP.ESMP_FieldRegridStore(srcFld, 
                                            dstFld, 
                                            srcMaskValues=maskVals, 
                                            dstMaskValues=None, 
                                            regridmethod=ESMP.ESMP_REGRIDMETHOD_CONSERVE, 
                                            unmappedaction=ESMP.ESMP_UNMAPPEDACTION_IGNORE, 
                                            srcFracField=srcFracField, 
                                            dstFracField=None)

        ESMP.ESMP_FieldRegrid(srcFld, dstFld, regrid)

        # get the cell areas
        ESMP.ESMP_FieldRegridGetArea(srcAreaField)
        ESMP.ESMP_FieldRegridGetArea(dstAreaField)
        srcarea = ESMP.ESMP_FieldGetPtr(srcAreaField)
        dstarea = ESMP.ESMP_FieldGetPtr(dstAreaField)

        srcFracPtr = ESMP.ESMP_FieldGetPtr(srcFracField)

        # check conservation
        marr = numpy.array(mask == 0, dtype = numpy.int32)
        srcFldSum, dstFldSum = srcFieldPtr.sum(), dstFieldPtr.sum()
        srcFldIntegral = (srcFieldPtr * srcarea * srcFracPtr).sum()
        dstFldIntegral = (dstFieldPtr*dstarea).sum()

        lackConservLocal = srcFldIntegral - dstFldIntegral

        print '[%d] src corner lo = %s hi = %s dst corner lo = %s hi = %s' % (rk, 
                                                                              str(srcLoCorner), 
                                                                              str(srcHiCorner), 
                                                                              str(dstLoCorner), 
                                                                              str(dstHiCorner)) 
        print '[%d] src center lo = %s hi = %s dst center lo = %s hi = %s' % (rk, 
                                                                              str(srcLoCenter), 
                                                                              str(srcHiCenter), 
                                                                              str(dstLoCenter), 
                                                                              str(dstHiCenter)) 
                                                                              
        print '[%d] checksum of src: %f checksum of dst: %f' % (rk, srcFldSum, dstFldSum)
        print '[%d] src total area integral: %g dst total area integral: %g diff: %g\n' % \
            (rk, srcFldIntegral, dstFldIntegral, lackConservLocal)

        lackConserv = MPI.COMM_WORLD.reduce(lackConservLocal, op=MPI.SUM, root=0)
        
        if rk == 0:
            print '[0] total lack of conservation (should be small): %f' % lackConserv
            assert(abs(lackConserv) < 1.e-6)

        # cleanup
        ESMP.ESMP_FieldRegridRelease(regrid)
        ESMP.ESMP_FieldDestroy(srcFld)
        ESMP.ESMP_FieldDestroy(dstFld)
        ESMP.ESMP_GridDestroy(srcGrid)
        ESMP.ESMP_GridDestroy(dstGrid)

if __name__ == '__main__':

    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(TestEsmpSmall)
    unittest.TextTestRunner(verbosity = 1).run(suite)

        
