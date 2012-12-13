"""
$Id: testEsmf3D.py 2387 2012-07-24 17:07:48Z dkindig $
3D Bilinear test of ESMP through esmf, regrid, and standalone
With and without masking
"""

import operator
import cdms2
import regrid2
import unittest
import ESMP
from regrid2 import esmf
import numpy
import time
import os
import sys
import re

def makeGrid(nx,ny,nz):
        dims = (nx, ny, nz)
        xbot, xtop, ybot, ytop, zbot, ztop = 0,4,1,5,.5,6
        x = numpy.linspace(xbot, xtop, nx)
        y = numpy.linspace(ybot, ytop, ny)
        z = numpy.linspace(zbot, ztop, nz)

        xxx = numpy.outer(x, numpy.outer(numpy.ones(ny), numpy.ones(nz))).reshape(dims)
        yyy = numpy.outer(numpy.ones(nx), numpy.outer(y, numpy.ones(nz))).reshape(dims)
        zzz = numpy.outer(numpy.ones(nx), numpy.outer(numpy.ones(ny), z)).reshape(dims)

        theVolume = [xxx, yyy, zzz]

        theData = xxx * yyy + zzz

        return dims, theVolume, theData

def addMask(theVolume):
    vol =numpy.ma.array(theVolume)
    vol[1,1,1] = numpy.ma.masked
    return vol

def convertToXYZ(lon, lat, lev, radius = 3671000):
    """
    @param lon numpy array k,j,i order
    @param lat numpy array k,j,i order
    @param lev numpy array k,j,i order, Assume positive down
    @param radius Earth in meters
    """
    cos, sin, pi = numpy.cos, numpy.sin, numpy.pi

    # Normalize to one
    r = (radius - lev)/radius 

    d2r = pi / 180.

    x = r * sin(lat * d2r) * cos(lon * d2r)
    y = r * sin(lat * d2r) * sin(lon * d2r)
    z = r * cos(lat * d2r)

    print x.min(), x.max(), y.min(), y.max(), z.min(), z.max(), r.min(), r.max()

    return (x, y, z)

class TestESMPRegridderConserve(unittest.TestCase):
    def setUp(self):
        """
        Unit test set up
        """
        pass
    def test_3D_Native(self):
        print 'running test_3d_esmf_native...'
        f = cdms2.open(sys.prefix + \
			       '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = f('so')[0, ...]

        # Coords
        soLats = so.getLatitude()[:]
        soLons = so.getLongitude()[:]
        soLevs = so.getLevel()[:]
        soX1 = numpy.ones(soLats.shape)
        soZ1 = numpy.ones(soLevs.shape)
        soLevs = numpy.outer(soLevs, soX1).reshape(so.shape)
        soLats = numpy.outer(soZ1, soLats).reshape(so.shape)
        soLons = numpy.outer(soZ1, soLons).reshape(so.shape)
        shape1 = tuple([i+1 for i in so.shape])
        shape = so.shape

        print 'Source Center'
        srcXYZCenter = convertToXYZ(soLons, soLats, soLevs)
 
        # Bounds
        soLtBd0 = so.getLatitude().getBounds()
        soLnBd0 = so.getLongitude().getBounds()
        soLvBd0 = so.getLevel().getBounds()
        
        soLtBd1 = numpy.zeros(shape1, dtype = numpy.float32)
        soLnBd1 = numpy.zeros(shape1, dtype = numpy.float32)
        soLvBd1 = numpy.zeros(shape1, dtype = numpy.float32)

        soLtBd1[:, :shape[1], :shape[2]] = soLtBd0[:,:,0]
        soLtBd1[:,  shape[1], :shape[2]] = soLtBd0[shape[1]-1,:,3]
        soLtBd1[:, :shape[1],  shape[2]] = soLtBd0[:,shape[2]-1,1]
        soLtBd1[:,  shape[1],  shape[2]] = soLtBd0[shape[1]-1,shape[2]-1,2]

        soLnBd1[:, :shape[1], :shape[2]] = soLnBd0[:,:,0]
        soLnBd1[:,  shape[1], :shape[2]] = soLnBd0[shape[1]-1,:,3]
        soLnBd1[:, :shape[1],  shape[2]] = soLnBd0[:,shape[2]-1,1]
        soLnBd1[:,  shape[1],  shape[2]] = soLnBd0[shape[1]-1,shape[2]-1,2]

        soLvBd2 = numpy.zeros(shape1[0], dtype = numpy.float32)
        soLvBd2[:-1] = soLvBd0[:,0]
        soLvBd2[-1] = soLvBd0[-1,1]
        soLvBd1 = numpy.outer(soLvBd2, numpy.ones(shape1[1:])).reshape(shape1)

        print 'Source Corner'
        srcXYZCorner = convertToXYZ(soLtBd1, soLnBd1, soLvBd1)

        clt = cdms2.open(sys.prefix + 'sample_data/clt.nc')('clt')[0, :, :]
        cltBounds = clt.getGrid().getBounds()

        # Destination grid dimensions 
        nz = 10
        ny, nx = clt.shape
        shape = (nz, ny, nx)
        nz1,ny1,nx1 = nz+1, ny+1, nx+1
        shape1 = (nz1, ny1, nx1)

        # Corners (Nodes) - Destination
        zb = numpy.linspace(0., 5000., nz1)
        yb = numpy.zeros((ny1,), numpy.float32)
        yb[:ny] = cltBounds[0][:, 0]
        yb[ny] = cltBounds[0][ny-1, 1]
        xb = numpy.zeros((nx1,), numpy.float32)
        xb[:nx] = cltBounds[1][:, 0]
        xb[nx] = cltBounds[1][nx-1, 1] 

        # Centers (Cells) - Destination
        zint = (zb[1]-zb[0])/2
        z =  numpy.linspace(zint, 5000-zint, nz)
        y = clt.getLatitude()
        x = clt.getLongitude()
        z1 = numpy.ones( (nz1,), numpy.float32 )
        y1 = numpy.ones( (ny1,), numpy.float32 )
        x1 = numpy.ones( (nx1,), numpy.float32 )

        dstLevCorner = numpy.outer(zb, numpy.outer(y1,x1)).reshape((shape1))
        dstLatCorner = numpy.outer(z1, numpy.outer(yb,x1)).reshape((shape1))
        dstLonCorner = numpy.outer(z1, numpy.outer(y1,xb)).reshape((shape1))

        print 'Desintation Corner'
        dstXYZCorner = convertToXYZ(dstLonCorner, dstLatCorner,
                                    dstLevCorner)
                                            
        # make Centers curvilinear
        z1 = numpy.ones( (nz,), numpy.float32 )
        y1 = numpy.ones( (ny,), numpy.float32 )
        x1 = numpy.ones( (nx,), numpy.float32 )

        dstLevCenter = numpy.outer(z, numpy.outer(y1,x1)).reshape((shape))
        dstLatCenter = numpy.outer(z1, numpy.outer(y,x1)).reshape((shape))
        dstLonCenter = numpy.outer(z1, numpy.outer(y1,x)).reshape((shape))

        print 'Desintation Center'
        dstXYZCenter = convertToXYZ(dstLonCenter, dstLatCenter,
                                    dstLevCenter)

        tic = time.time()

        dstShapeCorner = dstXYZCorner[0].shape
        dstShapeCenter = dstXYZCenter[0].shape
        srcShapeCorner = srcXYZCorner[0].shape
        srcShapeCenter = srcXYZCenter[0].shape

        # Establish the destination grid
        maxIndex = numpy.array(dstShapeCenter[::-1], dtype = numpy.int32)
        dstGrid3D = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, 
                                         coordSys = ESMP.ESMP_COORDSYS_CART)

        # Destination Corners
        ESMP.ESMP_GridAddCoord(dstGrid3D, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)

        dstDimsCorner = ESMP.ESMP_GridGetCoord(dstGrid3D, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        dstXCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid3D, 0,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        dstYCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid3D, 1,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        dstZCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid3D, 2,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        dstNtotCorner = reduce(operator.mul, 
                               [dstDimsCorner[1][i] - dstDimsCorner[0][i] \
                                    for i in range(3)])
        dstICornerBE = slice(dstDimsCorner[0][0], dstDimsCorner[1][0])
        dstJCornerBE = slice(dstDimsCorner[0][1], dstDimsCorner[1][1])
        dstKCornerBE = slice(dstDimsCorner[0][2], dstDimsCorner[1][2])
        dstXCorner[:] = dstXYZCorner[0][dstKCornerBE, 
                                     dstJCornerBE, 
                                     dstICornerBE].reshape(dstNtotCorner)
        dstYCorner[:] = dstXYZCorner[1][dstKCornerBE,
                                     dstJCornerBE, 
                                     dstICornerBE].reshape(dstNtotCorner)
        dstZCorner[:] = dstXYZCorner[2][dstKCornerBE,
                                     dstJCornerBE, 
                                     dstICornerBE].reshape(dstNtotCorner)
        # Destination Centers
        ESMP.ESMP_GridAddCoord(dstGrid3D, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)

        dstDimsCenter = ESMP.ESMP_GridGetCoord(dstGrid3D, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        dstXCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid3D, 0,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        dstYCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid3D, 1,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        dstZCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid3D, 2,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)

        # Dimensions for local processor
        dstNtotCenter = reduce(operator.mul, 
                               [dstDimsCenter[1][i] - dstDimsCenter[0][i] \
                                    for i in range(3)])
        dstICenterBE = slice(dstDimsCenter[0][0], dstDimsCenter[1][0])
        dstJCenterBE = slice(dstDimsCenter[0][1], dstDimsCenter[1][1])
        dstKCenterBE = slice(dstDimsCenter[0][2], dstDimsCenter[1][2])
        dstKJIShape = ((dstDimsCenter[1][2] - dstDimsCenter[0][2]),
                       (dstDimsCenter[1][1] - dstDimsCenter[0][1]),
                       (dstDimsCenter[1][0] - dstDimsCenter[0][0]))

        # Populate Destination Grid
        dstXCenter[:] = dstXYZCenter[0][dstKCenterBE, 
                                             dstJCenterBE, dstICenterBE].flat
        dstYCenter[:] = dstXYZCenter[1][dstKCenterBE,
                                             dstJCenterBE, dstICenterBE].flat
        dstZCenter[:] = dstXYZCenter[2][dstKCenterBE, 
                                             dstJCenterBE, dstICenterBE].flat

        # Create and Populate Destination Field
        dstField = ESMP.ESMP_FieldCreateGrid(dstGrid3D, 'dst_salinity',
                                  staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                                  typekind = ESMP.ESMP_TYPEKIND_R4)
        dstFieldPtr = ESMP.ESMP_FieldGetPtr(dstField)
        dstFieldPtr[:] = 1.

        # Establish the source grid
        maxIndex = numpy.array(so.shape[::-1], dtype = numpy.int32)
        srcGrid3D = ESMP.ESMP_GridCreateNoPeriDim(maxIndex, 
                                         coordSys = ESMP.ESMP_COORDSYS_CART)

        # Source Corners
        ESMP.ESMP_GridAddCoord(srcGrid3D, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)

        srcDimsCorner = ESMP.ESMP_GridGetCoord(srcGrid3D, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        srcXCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid3D, 0,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        srcYCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid3D, 1,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)
        srcZCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid3D, 2,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE)

        # Dimensions for local processor
        srcNtotCorner = reduce(operator.mul, 
                               [srcDimsCorner[1][i] - srcDimsCorner[0][i] \
                                    for i in range(3)])
        srcICornerBE = slice(srcDimsCorner[0][0], srcDimsCorner[1][0])
        srcJCornerBE = slice(srcDimsCorner[0][1], srcDimsCorner[1][1])
        srcKCornerBE = slice(srcDimsCorner[0][2], srcDimsCorner[1][2])
        srcKJIShape = ((srcDimsCorner[1][2] - srcDimsCorner[0][2]),
                       (srcDimsCorner[1][1] - srcDimsCorner[0][1]),
                       (srcDimsCorner[1][0] - srcDimsCorner[0][0]))

        # Populate the Source Grid Corners
        srcXCorner[:] = numpy.reshape(srcXYZCorner[0][srcKCornerBE, 
                                                  srcJCornerBE, 
                                                  srcICornerBE], srcNtotCorner)
        srcYCorner[:] = numpy.reshape(srcXYZCorner[1][srcKCornerBE,
                                                  srcJCornerBE, 
                                                  srcICornerBE], srcNtotCorner)
        srcZCorner[:] = numpy.reshape(srcXYZCorner[2][srcKCornerBE,
                                                  srcJCornerBE, 
                                                  srcICornerBE], srcNtotCorner)
        # Source Centers
        ESMP.ESMP_GridAddCoord(srcGrid3D, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)

        srcDimsCenter = ESMP.ESMP_GridGetCoord(srcGrid3D, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcXCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid3D, 0,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcYCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid3D, 1,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcZCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid3D, 2,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)

        # Dimensions for local processor
        srcNtotCenter = reduce(operator.mul, 
                               [srcDimsCenter[1][i] - srcDimsCenter[0][i] \
                                    for i in range(3)])
        srcICenterBE = slice(srcDimsCenter[0][0], srcDimsCenter[1][0])
        srcJCenterBE = slice(srcDimsCenter[0][1], srcDimsCenter[1][1])
        srcKCenterBE = slice(srcDimsCenter[0][2], srcDimsCenter[1][2])
        srcKJIShape = ((srcDimsCenter[1][2] - srcDimsCenter[0][2])-1,
                       (srcDimsCenter[1][1] - srcDimsCenter[0][1])-1,
                       (srcDimsCenter[1][0] - srcDimsCenter[0][0])-1)

        # Populate the Source Grid Centers
        srcXCenter[:] = numpy.reshape(srcXYZCenter[0][srcKCenterBE, 
                                                  srcJCenterBE, 
                                                  srcICenterBE], srcNtotCenter)
        srcYCenter[:] = numpy.reshape(srcXYZCenter[1][srcKCenterBE,
                                                  srcJCenterBE, 
                                                  srcICenterBE], srcNtotCenter)
        srcZCenter[:] = numpy.reshape(srcXYZCenter[2][srcKCenterBE,
                                                  srcJCenterBE, 
                                                  srcICenterBE], srcNtotCenter)
        # Source Mask
        ESMP.ESMP_GridAddItem(srcGrid3D, item = ESMP.ESMP_GRIDITEM_MASK)
        srcMaskPtr = ESMP.ESMP_GridGetItem(srcGrid3D, 
                                           item = ESMP.ESMP_GRIDITEM_MASK)
        srcMaskPtr[:] = numpy.reshape(so[srcKCornerBE,
                                              srcJCornerBE,
                                              srcICornerBE].mask,
                                              srcNtotCenter)

        # Create and Populate Source Field
        srcField = ESMP.ESMP_FieldCreateGrid(srcGrid3D, 'src_salinity',
                                  staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                                  typekind = ESMP.ESMP_TYPEKIND_R4)
        srcFieldPtr = ESMP.ESMP_FieldGetPtr(srcField)
        aa = numpy.array(so)
        srcFieldPtr[:] = numpy.reshape(aa[srcKCornerBE, 
                                          srcJCornerBE, 
                                          srcICornerBE], srcNtotCenter)

        srcMaskValues = numpy.array([1], numpy.int32)

        # Regrid
        regridOut = ESMP.ESMP_FieldRegridStore(srcField, dstField,
                               srcMaskValues = srcMaskValues,
                               dstMaskValues = None,
                               srcFracField = None,
                               dstFracField = None,
                               regridmethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                               unmappedaction = ESMP.ESMP_UNMAPPEDACTION_ERROR)
        ESMP.ESMP_FieldRegrid(srcField, dstField, regridOut)

        soInterp = numpy.reshape(dstFieldPtr, dstKJIShape)

        regridBck = ESMP.ESMP_FieldRegridStore(dstField, srcField,
                               srcMaskValues = None,
                               dstMaskValues = None,
                               srcFracField = None,
                               dstFracField = None,
                               regridmethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                               unmappedaction = ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        ESMP.ESMP_FieldRegrid(dstField, srcField, regridBck)

        soInterpInterp = numpy.reshape(srcFieldPtr, dstKJIShape)

        so.toVisit('so.vsh5')
        soInterp.toVisit('soInterp.vsh5')
        soInterpInterp.toVisit('soInterpInterp.vsh5')

    def Xtest_3D_cdat(self):
        soInterp = so.regrid(cltGrid, regridTool = 'esmp', 
                               regridMethod = 'bilinear')
        soInterpInterp = soInterp.regrid(soGrid, regridTool = 'esmp', 
                               regridMethod = 'bilinear')
        
        

if __name__ == '__main__':
    print "" # Spacer
    ESMP.ESMP_LogSet(True)
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(TestESMPRegridderConserve)
    unittest.TextTestRunner(verbosity = 1).run(suite)


