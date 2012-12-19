"""
$Id: testEsmf3DSmallesmf.py 2403 2012-07-31 23:07:16Z dkindig $
3D Bilinear test of ESMP through esmf, regrid, and standalone
With and without masking
"""

import cdms2  # ESMP_Initialize()
import unittest
import ESMP
from regrid2 import esmf
from regrid2 import ESMFRegrid
import numpy
from mpi4py import MPI

import re

ESMP.ESMP_LogSet(True)

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

# Order here is level, lat, lon as required by esmf.py
        theVolume = [zzz, yyy, xxx]
        theBounds = [zzzb, yyyb, xxxb]

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

    def test1_3D_esmf_Bilinear(self):
        srcDims, srcXYZCenter, srcData, srcBounds = makeGrid(5, 4, 3)
        dstDims, dstXYZCenter, dstData, dstBounds = makeGrid(5, 4, 3)

        # Establish the destination grid
        dstGrid3D = esmf.EsmfStructGrid(dstData.shape, 
                                        coordSys = ESMP.ESMP_COORDSYS_CART)
        dstGrid3D.setCoords(dstXYZCenter, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                              globalIndexing = True) 

        # Establish the Source grid
        srcGrid3D = esmf.EsmfStructGrid(srcData.shape, 
                                        coordSys = ESMP.ESMP_COORDSYS_CART)
        srcGrid3D.setCoords(srcXYZCenter, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                            globalIndexing = True) 

        # Create and populate the fields
        dstField = esmf.EsmfStructField(dstGrid3D, 'dstDataCtr', srcData.dtype,
                                    ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcField = esmf.EsmfStructField(srcGrid3D, 'srcDataCtr', srcData.dtype,
                                    ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcFldIn = esmf.EsmfStructField(srcGrid3D, 'srcDataInterp', srcData.dtype,
                                    ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcField.setLocalData(srcData, ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                              globalIndexing = True)
        dstField.setLocalData(dstData*0, ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                              globalIndexing = True)
        srcFldIn.setLocalData(srcData*0, ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                              globalIndexing = True)

        # Regrid
        regridOut = esmf.EsmfRegrid(srcField, dstField,
                               srcMaskValues = None,
                               dstMaskValues = None,
                               regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                               unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        regridOut()

        regridBck = esmf.EsmfRegrid(dstField, srcFldIn,
                               srcMaskValues = None,
                               dstMaskValues = None,
                               regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                               unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        regridBck()
        
        soInterp = dstField.getData(rootPe = self.rootPe)
        soInterpInterp = srcFldIn.getData(rootPe = self.rootPe)

        if self.pe == self.rootPe:
            minlsd, maxlsd = srcData.min(), srcData.max()
            minlsi, maxlsi = soInterp.min(), soInterp.max()
            minlii, maxlii = soInterpInterp.min(), soInterpInterp.max()
            self.assertEqual(minlsd, minlsi.round(2))
            self.assertEqual(minlsd, minlii.round(2))
            self.assertEqual(maxlsd, maxlsi.round(2))
            self.assertEqual(maxlsd, maxlii.round(2))

    def test2_3D_Native_Conserve(self):
        print
        srcDims, srcXYZCenter, srcData, srcBounds = makeGrid(5, 4, 3)
        dstDims, dstXYZCenter, dstData, dstBounds = makeGrid(5, 4, 3)

        # Establish the Destination grid
        dstGrid3D = esmf.EsmfStructGrid(dstData.shape, 
                                        coordSys = ESMP.ESMP_COORDSYS_CART,
                                        hasBounds = True)
        dstGrid3D.setCoords(dstXYZCenter, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                            globalIndexing = True) 
        dstGrid3D.setCoords(dstBounds, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE,
                            globalIndexing = True) 

        # Establish the Source grid
        srcGrid3D = esmf.EsmfStructGrid(srcData.shape, 
                                        coordSys = ESMP.ESMP_COORDSYS_CART,
                                        hasBounds = True)
        srcGrid3D.setCoords(srcXYZCenter, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                            globalIndexing = True) 
        srcGrid3D.setCoords(srcBounds, 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CORNER_VFACE,
                            globalIndexing = True) 

        # Create and populate the fields
        dstField = esmf.EsmfStructField(dstGrid3D, 'dstDataCtr', srcData.dtype,
                                    ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcField = esmf.EsmfStructField(srcGrid3D, 'srcDataCtr', srcData.dtype,
                                    ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcFldIn = esmf.EsmfStructField(srcGrid3D, 'srcDataInterp', srcData.dtype,
                                    ESMP.ESMP_STAGGERLOC_CENTER_VCENTER)
        srcField.setLocalData(srcData, ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                              globalIndexing = True)
        dstField.setLocalData(dstData*0, ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                              globalIndexing = True)
        srcFldIn.setLocalData(srcData*0, ESMP.ESMP_STAGGERLOC_CENTER_VCENTER,
                              globalIndexing = True)

        # Regrid
        regridOut = esmf.EsmfRegrid(srcField, dstField,
                               srcMaskValues = None,
                               dstMaskValues = None,
                               regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE,
                               unMappedAction = ESMP.ESMP_UNMAPPEDACTION_ERROR)
        regridOut()

        regridBck = esmf.EsmfRegrid(dstField, srcFldIn,
                               srcMaskValues = None,
                               dstMaskValues = None,
                               regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE,
                               unMappedAction = ESMP.ESMP_UNMAPPEDACTION_ERROR)
        regridBck()
        
        soInterp = dstField.getData(rootPe = self.rootPe)
        soInterpInterp = srcFldIn.getData(rootPe = self.rootPe)

#        soInterpSrcAreas = regridOut.getSrcAreas(self.rootPe)
#        soInterpDstAreas = regridOut.getDstAreas(self.rootPe)
#        soInterpSrcAreaFractions = regridOut.getSrcAreaFractions(self.rootPe)
#        soInterpDstAreaFractions = regridOut.getDstAreaFractions(self.rootPe)
#        soIntIntSrcAreas = regridBck.getSrcAreas(self.rootPe)
#        soIntIntDstAreas = regridBck.getDstAreas(self.rootPe)
#        soIntIntSrcAreaFractions = regridBck.getSrcAreaFractions(self.rootPe)
#        soIntIntDstAreaFractions = regridBck.getDstAreaFractions(self.rootPe)
#
#        srcInterpIntegral = (soInterpSrcAreas * srcData).sum()
#        dstInterpIntegral = (soInterpDstAreas * soInterp).sum()
#        interpConserve = srcInterpIntegral - dstInterpIntegral
#
#        lc1 = MPI.COMM_WORLD.reduce(interpConserve, op = MPI.SUM, root = self.rootPe)
#
        if self.pe == self.rootPe:
#            print numpy.histogram(srcData, [0,10.0000, 20.000, 30])
#            print numpy.histogram(soInterp, [0,10.0000, 20.000, 30])
#            srcInterpIntegral = (soInterpSrcAreas * srcData).sum()
#            dstInterpIntegral = (soInterpDstAreas * soInterp).sum()
#            interpConserve = srcInterpIntegral - dstInterpIntegral
#            srcIntIntIntegral = (soIntIntSrcAreas * soInterp).sum()
#            dstIntIntIntegral = (soIntIntDstAreas * soInterpInterp).sum()
#            intIntConserve = srcIntIntIntegral - dstIntIntIntegral
#
#            self.assertLess(lc1, 2.e-6)
#            self.assertLess(lc2, 2.e-6)
            minlsd, maxlsd = srcData.min(), srcData.max()
            minlsi, maxlsi = soInterp.min(), soInterp.max()
            minlii, maxlii = soInterpInterp.min(), soInterpInterp.max()

            self.assertEqual(minlsd, minlsi)
            self.assertEqual(maxlsd, maxlsi)

if __name__ == '__main__':
    print "" # Spacer
    ESMP.ESMP_LogSet(True)
    suite = unittest.TestLoader().loadTestsFromTestCase(TestESMPRegridderConserve)
    unittest.TextTestRunner(verbosity = 1).run(suite)
