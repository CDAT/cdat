"""
$Id: testEsmf3DSmallNative.py 2389 2012-07-26 15:51:43Z dkindig $
3D Bilinear test of ESMP through esmf, regrid, and standalone
With and without masking
"""

import operator
import cdms2
import regrid2
import unittest
import ESMP
from regrid2 import esmf
from regrid2 import ESMFRegrid
from regrid2 import GenericRegrid
import numpy
import time
import os
from mpi4py import MPI

import re

ESMP.ESMP_LogSet(True)

def esmfGrid(grid, cds, sloc):
    """
    @param grid ESMF grid
    @param cds list of coordinates
    @param sloc ESMP staggerlocation
    """
    # Destination Centers
    ESMP.ESMP_GridAddCoord(grid, staggerloc = sloc)

    lo, hi = ESMP.ESMP_GridGetCoord(grid, staggerloc = sloc)
    ndims = len(lo)
    # Dimensions for local processor
    Ntot = reduce(operator.mul, [hi[i] - lo[i] for i in range(ndims)])
    ijkBE = []
    shape = []
    for i in range(ndims):
        ind = (ndims-1)-i
        ijkBE.append(slice(lo[ind], hi[ind], None))
        shape.append(hi[ind] - lo[ind])
        
    print ijkBE
    xyzPtr = []
    for i in range(ndims):
        xyzPtr.append(ESMP.ESMP_GridGetCoordPtr(grid, i, staggerloc = sloc))
        xyzPtr[i][:] = cds[i][ijkBE].flat
    shape = tuple(shape)

    return xyzPtr, ijkBE, shape

class esmfField: #(grid, data, name, ijkBE, ijkShape, sloc, typekind):
    def __init__(self, grid, name, staggerloc, typekind):
        """
        @param grid ESMF grid
        @param name string
        @param sloc ESMP. 3D shape
        @param typekind. ESMP typekind 
        """

    def setData(self, data, slab):
        """
        @param data numpy array
        @param slab tuple of slices
        """
        fieldPtr = ESMP.ESMP_FieldGetPtr(self.field)
        fieldPtr[:] = data[slab].flat

        return fieldPtr

def makeGrid(nx,ny,nz):
        dims = (nz, ny, nx)
        dimb = (nz+1, ny+1,nx+1)
        xbot, xtop, ybot, ytop, zbot, ztop = 1,4,1,5,.5,6
        xbob, xtob, ybob, ytob, zbob, ztob = .5,4.5,.5,5.5,0,6.5
        x = numpy.linspace(xbot, xtop, nx)
        y = numpy.linspace(ybot, ytop, ny)
        z = numpy.linspace(zbot, ztop, nz)

        xb = numpy.linspace(xbob, xtob, nx+1)
        yb = numpy.linspace(ybob, ytob, ny+1)
        zb = numpy.linspace(zbob, ztob, nz+1)
        
        xx = numpy.outer(numpy.ones(ny), x)
        yy = numpy.outer(y, numpy.ones(nx))
        ones = numpy.outer(numpy.ones(ny), numpy.ones(nx))
        xxx = numpy.outer(numpy.ones(nz), xx).reshape(dims)
        yyy = numpy.outer(numpy.ones(nz), yy).reshape(dims)
        zzz = numpy.outer(z, ones).reshape(dims)

        xxb = numpy.outer(numpy.ones(ny+1), xb)
        yyb = numpy.outer(yb, numpy.ones(nx+1))
        ones = numpy.outer(numpy.ones(ny+1), numpy.ones(nx+1))
        xxxb = numpy.outer(numpy.ones(nz+1), xxb).reshape(dimb)
        yyyb = numpy.outer(numpy.ones(nz+1), yyb).reshape(dimb)
        zzzb = numpy.outer(zb, ones).reshape(dimb)

        theVolume = [xxx, yyy, zzz]
        theBounds = [xxxb, yyyb, zzzb]

        theData = xxx * yyy + zzz

        return dims, theVolume, theData, theBounds

class TestESMPRegridderConserve(unittest.TestCase):
    def setUp(self):
        """
        Unit test set up
        """
        self.pe = MPI.COMM_WORLD.Get_rank()
        self.np = MPI.COMM_WORLD.Get_size()
        self.rootPe = 0

    def Xtest1_3D_Native_Bilinear(self):
        print 'test1'
        srcDims, srcXYZCenter, srcData, srcBounds = makeGrid(5, 4, 3)
        dstDims, dstXYZCenter, dstData, dstBounds = makeGrid(5, 4, 3)

        # Initialize the grids **without** coordinates
        maxIndex = numpy.array(dstDims[::-1], dtype = numpy.int32)
        dstGrid3D = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, 
                                         coordSys = ESMP.ESMP_COORDSYS_CART)
        maxIndex = numpy.array(srcDims[::-1], dtype = numpy.int32)
        srcGrid3D = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, 
                                         coordSys = ESMP.ESMP_COORDSYS_CART)

        # Populate the Grids
        dstXYZCtrPtr, dstIJKbe, dstCtrShape = esmfGrid(dstGrid3D, dstXYZCenter, 
                                           ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        dstXYZCnrPtr, dstIJKbe, dstCnrShape = esmfGrid(dstGrid3D, dstBounds, 
                                           ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        srcXYZCtrPtr, srcIJKbe, srcCtrShape = esmfGrid(srcGrid3D, srcXYZCenter, 
                                           ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcXYZCnrPtr, srcIJKbe, srcCnrShape = esmfGrid(srcGrid3D, srcBounds, 
                                           ESMP.ESMP_STAGGERLOC_CORNER_VFACE)

        # initialize the fields **without** data, after ESMP_GridAddCoord
        dstField = ESMP.ESMP_FieldCreateGrid(dstGrid3D, 'dstDataCtr',
                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER, 
                       typekind = ESMP.ESMP_TYPEKIND_R4)
        srcField = ESMP.ESMP_FieldCreateGrid(srcGrid3D, 'srcDataCtr', 
                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                       typekind = ESMP.ESMP_TYPEKIND_R4)
        srcIntFd = ESMP.ESMP_FieldCreateGrid(srcGrid3D, 'srcDataCtr', 
                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                       typekind = ESMP.ESMP_TYPEKIND_R4)

        # Populate the fields
        dstFieldPtr = ESMP.ESMP_FieldGetPtr(dstField)
        dstFieldPtr[:] = 0
        srcFieldPtr = ESMP.ESMP_FieldGetPtr(srcField)
        srcFieldPtr[:] = srcData[srcIJKbe].flat
        srcIntFdPtr = ESMP.ESMP_FieldGetPtr(srcIntFd)
        srcIntFdPtr[:] = -1

        srcMaskValues = numpy.array([1], numpy.int32)
        # Regrid
        regridOut = ESMP.ESMP_FieldRegridStore(srcField, dstField,
                               srcMaskValues = srcMaskValues,
                               dstMaskValues = None,
                               srcFracField = None,
                               dstFracField = None,
                               regridmethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                               unmappedaction = ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        ESMP.ESMP_FieldRegrid(srcField, dstField, regridOut)

        regridBck = ESMP.ESMP_FieldRegridStore(dstField, srcIntFd,
                               srcMaskValues = None,
                               dstMaskValues = None,
                               srcFracField = None,
                               dstFracField = None,
                               regridmethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                               unmappedaction = ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        ESMP.ESMP_FieldRegrid(dstField, srcIntFd, regridBck)

        minlsd, maxlsd = srcData.min(), srcData.max()
        minlsi, maxlsi = dstFieldPtr.min(), dstFieldPtr.max()
        minlii, maxlii = srcIntFdPtr.min(), srcIntFdPtr.max()
        minsd = MPI.COMM_WORLD.reduce(minlsd, op = MPI.MIN, root = self.rootPe)
        maxsd = MPI.COMM_WORLD.reduce(maxlsd, op = MPI.MAX, root = self.rootPe)
        minsi = MPI.COMM_WORLD.reduce(minlsi, op = MPI.MIN, root = self.rootPe)
        maxsi = MPI.COMM_WORLD.reduce(maxlsi, op = MPI.MAX, root = self.rootPe)
        minii = MPI.COMM_WORLD.reduce(minlii, op = MPI.MIN, root = self.rootPe)
        maxii = MPI.COMM_WORLD.reduce(maxlii, op = MPI.MAX, root = self.rootPe)
        
        if self.pe == self.rootPe:
            print(minsd, minsi)
            print(minsd, minii)
            print(maxsd, maxsi)
            print(maxsd, maxii)
            self.assertEqual(minsd, minsi)
            self.assertEqual(minsd, minii)
            self.assertEqual(maxsd, maxsi)
            self.assertEqual(maxsd, maxii)

    def Xtest2_3D_Native_Conserve(self):
        srcDims, srcXYZCenter, srcData, srcBounds = makeGrid(5, 4, 3)
        dstDims, dstXYZCenter, dstData, dstBounds = makeGrid(5, 4, 3)

        # Establish the destination grid
        maxIndex = numpy.array(dstDims[::-1], dtype = numpy.int32)
        dstGrid3D = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, 
                                         coordSys = ESMP.ESMP_COORDSYS_CART)
        maxIndex = numpy.array(srcDims[::-1], dtype = numpy.int32)
        srcGrid3D = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, 
                                         coordSys = ESMP.ESMP_COORDSYS_CART)

        dstXYZCtrPtr, dstIJKCtrbe, dstCtrShape = esmfGrid(dstGrid3D, dstXYZCenter, 
                                           ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcXYZCtrPtr, srcIJKCtrbe, srcCtrShape = esmfGrid(srcGrid3D, srcXYZCenter, 
                                           ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        dstXYZCnrPtr, dstIJKCnrbe, dstCnrShape = esmfGrid(dstGrid3D, dstBounds, 
                                           ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        srcXYZCnrPtr, srcIJKCnrbe, srcCnrShape = esmfGrid(srcGrid3D, srcBounds, 
                                           ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        
        # initialize the fields **without** data
        dstField = esmfField(dstGrid3D, 'dstDataCtr', 
                             ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                             ESMP.ESMP_TYPEKIND_R4)
        srcField = esmfField(srcGrid3D, 'srcDataCtr', 
                             ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                             ESMP.ESMP_TYPEKIND_R4)
        srcIntFd = esmfField(srcGrid3D, 'srcDataCtr', 
                             ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                             ESMP.ESMP_TYPEKIND_R4)

        # Populate the fields
        dstFieldPtr = dstField.setData(dstData*0, dstIJKCtrbe)
        srcFieldPtr = srcField.setData(srcData, srcIJKCtrbe)
        srcIntFdPtr = srcIntFd.setData(srcData, srcIJKCtrbe)

        srcIntFdPtr[:] = -1
        
        soInterpInterp = numpy.reshape(srcIntFdPtr, srcCtrShape)
        
        srcMaskValues = None
        # Regrid
        regridOut = ESMP.ESMP_FieldRegridStore(srcField.field, dstField.field,
                               srcMaskValues = srcMaskValues,
                               dstMaskValues = None,
                               srcFracField = None,
                               dstFracField = None,
                               regridmethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE,
                               unmappedaction = ESMP.ESMP_UNMAPPEDACTION_ERROR)
        ESMP.ESMP_FieldRegrid(srcField.field, dstField.field, regridOut)
        
        soInterp = numpy.reshape(dstFieldPtr, dstCtrShape)
        
        regridBck = ESMP.ESMP_FieldRegridStore(dstField.field, srcIntFd.field,
                               srcMaskValues = None,
                               dstMaskValues = None,
                               srcFracField = None,
                               dstFracField = None,
                               regridmethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE,
                               unmappedaction = ESMP.ESMP_UNMAPPEDACTION_ERROR)
        ESMP.ESMP_FieldRegrid(dstField.field, srcIntFd.field, regridBck)
        
        soInterpInterp = numpy.reshape(srcIntFdPtr, srcCtrShape)
        
        minlsd, maxlsd = srcData.min(), srcData.max()
        minlsi, maxlsi = soInterp.min(), soInterp.max()
        minlii, maxlii = soInterpInterp.min(), soInterpInterp.max()
        minsd = MPI.COMM_WORLD.reduce(minlsd, op = MPI.MIN, root = self.rootPe)
        maxsd = MPI.COMM_WORLD.reduce(maxlsd, op = MPI.MAX, root = self.rootPe)
        minsi = MPI.COMM_WORLD.reduce(minlsi, op = MPI.MIN, root = self.rootPe)
        maxsi = MPI.COMM_WORLD.reduce(maxlsi, op = MPI.MAX, root = self.rootPe)
        minii = MPI.COMM_WORLD.reduce(minlii, op = MPI.MIN, root = self.rootPe)
        maxii = MPI.COMM_WORLD.reduce(maxlii, op = MPI.MAX, root = self.rootPe)
        
        if self.pe == self.rootPe:
        
            self.assertEqual(minsd, minsi)
            self.assertEqual(minsd, minii)
            self.assertEqual(maxsd, maxsi)
            self.assertEqual(maxsd, maxii)

    def Xtest3_mvESMFRegrid_pregenSlabs(self):
        print '\ntest3'
        srcDims, srcXYZCenter, srcData, srcBounds = makeGrid(5, 4, 3)
        dstDims, dstXYZCenter, dstData, dstBounds = makeGrid(5, 4, 3)
                        
        ro = ESMFRegrid(srcXYZCenter[0].shape, dstXYZCenter[0].shape, srcData.dtype,
                        'conserve','center', 0,'cart', staggerloc = 'center',
                        hasSrcBounds = True, 
                        hasDstBounds = True)
        srcSlab = ro.getSrcLocalSlab('center')
        dstSlab = ro.getDstLocalSlab('center')
        srcCds = [coord[srcSlab] for coord in srcXYZCenter]
        dstCds = [coord[dstSlab] for coord in dstXYZCenter]
        srcSlab = ro.getSrcLocalSlab('vface')
        dstSlab = ro.getDstLocalSlab('vface')
        srcBnd = [bound[srcSlab] for bound in srcBounds]
        dstBnd = [bound[dstSlab] for bound in dstBounds]

        ro.setCoords(srcCds, dstCds, 
                     srcBounds = srcBnd, dstBounds = dstBnd)
        ro.computeWeights()

        srcDataSec = numpy.array(srcData[srcSlab], srcData.dtype)
        dstDataSec = numpy.array(dstData[dstSlab], dstData.dtype)

        ro.apply(srcDataSec, dstDataSec, rootPe = None)

        print 'src', srcDataSec.min(), srcDataSec.max(), srcDataSec.shape
        print 'dst', dstDataSec.min(), dstDataSec.max(), dstDataSec.shape

        sA = ro.getSrcAreas(rootPe = self.rootPe)
        dA = ro.getDstAreas(rootPe = self.rootPe)
        sF = ro.getSrcAreaFractions(rootPe = self.rootPe)
        dF = ro.getDstAreaFractions(rootPe = self.rootPe)

    def Xtest4_mvESMFRegrid_esmfSlabs(self):
        print '\ntest4'
        srcDims, srcXYZCenter, srcData, srcBounds = makeGrid(5, 4, 3)
        dstDims, dstXYZCenter, dstData, dstBounds = makeGrid(5, 4, 3)
                        
        ro = ESMFRegrid(srcXYZCenter[0].shape, dstXYZCenter[0].shape, srcData.dtype,
                        'conserve','center', 0,'cart', staggerloc = 'center',
                        hasSrcBounds = True,
                        hasDstBounds = True)
        ro.setCoords(srcXYZCenter, dstXYZCenter, 
                     srcBounds = srcBounds, dstBounds = dstBounds,
                     globalIndexing = True)
        ro.computeWeights()

        ro.apply(srcData, dstData, rootPe = self.rootPe, globalIndexing = True)
        print 'src', srcData.min(), srcData.max(), srcData.shape
        print 'dst', dstData.min(), dstData.max(), dstData.shape

        sA = ro.getSrcAreas(rootPe = self.rootPe)
        dA = ro.getDstAreas(rootPe = self.rootPe)
        sF = ro.getSrcAreaFractions(rootPe = self.rootPe)
        dF = ro.getDstAreaFractions(rootPe = self.rootPe)

    def test5_mvGenericRegrid(self):
        print '\ntest5'
        srcDims, srcXYZCenter, srcData, srcBounds = makeGrid(5, 4, 3)
        dstDims, dstXYZCenter, dstData, dstBounds = makeGrid(5, 4, 3)
                        
        ro = GenericRegrid(srcXYZCenter, dstXYZCenter, srcData.dtype,
                        'conserve','esmf', staggerloc = 'center', 
                        periodicity = 0, coordSys = 'cart',
                        srcBounds = srcBounds, dstBounds = dstBounds)
        ro.computeWeights()

        diag = {'srcAreas':None, 'srcAreaFractions':None, 
                'dstAreas':None, 'dstAreaFractions':None}
        ro.apply(srcData, dstData, rootPe = self.rootPe)
        print 'src', srcData.min(), srcData.max(), srcData.shape
        print 'dst', dstData.min(), dstData.max(), dstData.shape

        sA = diag['srcAreas']
        dA = diag['dstAreas']
        sF = diag['srcAreaFractions']
        dF = diag['dstAreaFractions']

if __name__ == '__main__':
    print "" # Spacer
    ESMP.ESMP_LogSet(True)
    suite = unittest.TestLoader().loadTestsFromTestCase(TestESMPRegridderConserve)
    unittest.TextTestRunner(verbosity = 1).run(suite)
