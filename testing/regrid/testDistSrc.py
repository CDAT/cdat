"""
$Id: testDistSrc.py 2410 2012-08-10 16:05:07Z dkindig $

Test interpolation on salinity datasets

"""

import time
import re
import numpy
import cdms2
import regrid2
import unittest
import ESMP
from regrid2 import esmf
from matplotlib import pylab
from mpi4py import MPI
import scipy.io.netcdf
from regrid2 import ESMFRegrid
import sys

CENTER = ESMP.ESMP_STAGGERLOC_CENTER
CORNER = ESMP.ESMP_STAGGERLOC_CORNER
R8 = ESMP.ESMP_TYPEKIND_R8
R4 = ESMP.ESMP_TYPEKIND_R4

def _buildCorners(bounds):
    """
    Return an array of bounds converted from [x, [y], nDims] -> x+1, [y+1]
    @param bounds CdmsVar.getBounds()
    @return ndarrray of bounds
    """

    bndShape = [s+1 for s in bounds.shape[:-1]]
    bnd = numpy.ones(bndShape, dtype = bounds.dtype)
    if len(bndShape) == 1:
        bnd[:-1] = bounds[..., 0]
        bnd[ -1] = bounds[ -1, 1]
    elif len(bndShape) > 1:
        bnd[:-1, :-1] = bounds[  :,  :, 0]
        bnd[:-1,  -1] = bounds[  :, -1, 1]
        bnd[ -1,  -1] = bounds[ -1, -1, 2]
        bnd[ -1, :-1] = bounds[ -1,  :, 3]

    return bnd

def _getCorners(coordBounds):
    """
    Return a list of bounds built from a list of coordinates
    @param coordBounds boundary coordinate list
    @return [latBounds, lonBounds]
    """
    bounds = []
    for c in coordBounds:
        bnds = _buildCorners(c)
        bounds.append(bnds)

    return bounds


class Test(unittest.TestCase):

    def setUp(self):
        self.pe = MPI.COMM_WORLD.Get_rank()
        self.nprocs = MPI.COMM_WORLD.Get_size()

    def Xtest0_ESMP(self):

        import scipy.io.netcdf

        #
        # 1. input
        #

        coordSys = ESMP.ESMP_COORDSYS_SPH_DEG # ESMP.ESMP_COORDSYS_CART fails

        inFile = sys.prefix + \
            '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc'
        srcF = scipy.io.netcdf.netcdf_file(inFile)
        #so = srcF.variables['so'][0, 0,...]
        missing_value = 1.e20
        srcGrd = [srcF.variables['lat'][:], srcF.variables['lon'][:]]
        srcBounds = _getCorners([srcF.variables['lat_vertices'][:], srcF.variables['lon_vertices'][:]])

        lat1dBounds = numpy.arange(-90.0, 90.001, 5.0)
        lon1dBounds = numpy.arange(-180.0, 180.001, 5.0)
        lat1dCenter = 0.5*(lat1dBounds[:-1] + lat1dBounds[1:])
        lon1dCenter = 0.5*(lon1dBounds[:-1] + lon1dBounds[1:])
        lat2dBounds = numpy.outer(lat1dBounds,
                                  numpy.ones( (len(lon1dBounds),), lon1dBounds.dtype ) )
        lon2dBounds = numpy.outer(numpy.ones( (len(lat1dBounds),),
                                              lat1dBounds.dtype ), lon1dBounds )
        lat2dCenter = numpy.outer(lat1dCenter,
                                  numpy.ones( (len(lon1dCenter),), lon1dCenter.dtype ) )
        lon2dCenter = numpy.outer(numpy.ones( (len(lat1dCenter),),
                                              lat1dCenter.dtype ), lon1dCenter )
        dstGrd = [lat2dCenter, lon2dCenter]
        dstBounds = [lat2dBounds, lon2dBounds]


        srcDims = srcGrd[0].shape
        dstDims = dstGrd[0].shape

        srcMaxIndex = numpy.array(srcDims[::-1], numpy.int32) # number of cells
        dstMaxIndex = numpy.array(dstDims[::-1], numpy.int32) # number of cells

        #
        # 2. create grid objects
        #

        srcGrid = ESMP.ESMP_GridCreateNoPeriDim(srcMaxIndex, coordSys = coordSys)
        dstGrid = ESMP.ESMP_GridCreateNoPeriDim(dstMaxIndex, coordSys = coordSys)

        ESMP.ESMP_GridAddCoord(srcGrid, staggerloc = CORNER)
        ESMP.ESMP_GridAddCoord(srcGrid, staggerloc = CENTER)
        ESMP.ESMP_GridAddCoord(dstGrid, staggerloc = CORNER)
        ESMP.ESMP_GridAddCoord(dstGrid, staggerloc = CENTER)

        # add masks
        ESMP.ESMP_GridAddItem(srcGrid, item=ESMP.ESMP_GRIDITEM_MASK)
        srcGridMaskPtr = ESMP.ESMP_GridGetItem(srcGrid, item=ESMP.ESMP_GRIDITEM_MASK)

        ESMP.ESMP_GridAddItem(dstGrid, item=ESMP.ESMP_GRIDITEM_MASK)
        dstGridMaskPtr = ESMP.ESMP_GridGetItem(dstGrid, item=ESMP.ESMP_GRIDITEM_MASK)

        srcLoCorner, srcHiCorner = ESMP.ESMP_GridGetCoord(srcGrid, CORNER)
        srcLoCenter, srcHiCenter = ESMP.ESMP_GridGetCoord(srcGrid, CENTER)
        dstLoCorner, dstHiCorner = ESMP.ESMP_GridGetCoord(dstGrid, CORNER)
        dstLoCenter, dstHiCenter = ESMP.ESMP_GridGetCoord(dstGrid, CENTER)

        srcNx1 = srcHiCorner[0] - srcLoCorner[0]
        srcNy1 = srcHiCorner[1] - srcLoCorner[1]
        srcNx = srcHiCenter[0] - srcLoCenter[0]
        srcNy = srcHiCenter[1] - srcLoCenter[1]

        dstNx1 = dstHiCorner[0] - dstLoCorner[0]
        dstNy1 = dstHiCorner[1] - dstLoCorner[1]
        dstNx = dstHiCenter[0] - dstLoCenter[0]
        dstNy = dstHiCenter[1] - dstLoCenter[1]

        srcXCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid, 0, CORNER)
        srcYCorner = ESMP.ESMP_GridGetCoordPtr(srcGrid, 1, CORNER)

        srcXCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid, 0, CENTER)
        srcYCenter = ESMP.ESMP_GridGetCoordPtr(srcGrid, 1, CENTER)

        dstXCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid, 0, CORNER)
        dstYCorner = ESMP.ESMP_GridGetCoordPtr(dstGrid, 1, CORNER)

        dstXCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid, 0, CENTER)
        dstYCenter = ESMP.ESMP_GridGetCoordPtr(dstGrid, 1, CENTER)

        #
        # 3. create fields
        #

        srcFld = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFld',
                                           typekind = R8, staggerloc = CENTER)
        dstFld = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstFld',
                                           typekind = R8, staggerloc = CENTER)
        srcAreaField = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcAreas',
                                                 typekind = R8, staggerloc = CENTER)
        dstAreaField = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstAreas',
                                                 typekind = R8, staggerloc = CENTER)
        srcFracField = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFracAreas',
                                                 typekind = R8, staggerloc = CENTER)
        dstFracField = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstFracAreas',
                                                 typekind = R8, staggerloc = CENTER)
        srcFieldPtr = ESMP.ESMP_FieldGetPtr(srcFld)
        srcFracPtr = ESMP.ESMP_FieldGetPtr(srcFracField)
        dstFieldPtr = ESMP.ESMP_FieldGetPtr(dstFld)
        dstFracPtr = ESMP.ESMP_FieldGetPtr(dstFracField)

        #
        # 4. set the coordinates and the fields
        #

        srcYCorner[:] = srcBounds[0][srcLoCorner[1]:srcHiCorner[1], srcLoCorner[0]:srcHiCorner[0]].flat
        srcXCorner[:] = srcBounds[1][srcLoCorner[1]:srcHiCorner[1], srcLoCorner[0]:srcHiCorner[0]].flat

        # src-center coordinates, field, and mask
        yc = numpy.array(srcGrd[0][srcLoCenter[1]:srcHiCenter[1], srcLoCenter[0]:srcHiCenter[0]])
        xc = numpy.array(srcGrd[1][srcLoCenter[1]:srcHiCenter[1], srcLoCenter[0]:srcHiCenter[0]])
        srcYCenter[:] = yc.flat
        srcXCenter[:] = xc.flat
        # read the local data
        soLocalToPe = srcF.variables['so'][0, 0, srcLoCenter[1]:srcHiCenter[1], srcLoCenter[0]:srcHiCenter[0]]
        msk = numpy.array(soLocalToPe == missing_value, numpy.int32)
        srcGridMaskPtr[:] = msk.flat
        fld = numpy.array(soLocalToPe, soLocalToPe.dtype)
        # set to zero where masked
        fld *= (1 - msk)
        srcFieldPtr[:] = fld.flat
        srcFracPtr[:] = 1.0

        dstYCorner[:] = dstBounds[0][dstLoCorner[1]:dstHiCorner[1], dstLoCorner[0]:dstHiCorner[0]].flat
        dstXCorner[:] = dstBounds[1][dstLoCorner[1]:dstHiCorner[1], dstLoCorner[0]:dstHiCorner[0]].flat

        # dst-center coordinates, field, and mask
        yc = numpy.array(dstGrd[0][dstLoCenter[1]:dstHiCenter[1], dstLoCenter[0]:dstHiCenter[0]])
        xc = numpy.array(dstGrd[1][dstLoCenter[1]:dstHiCenter[1], dstLoCenter[0]:dstHiCenter[0]])
        dstYCenter[:] = yc.flat
        dstXCenter[:] = xc.flat
        dstGridMaskPtr[:] = 0
        dstFieldPtr[:] = missing_value
        dstFracPtr[:] = 1.0

        #
        # 5. interpolate
        #

        maskVals = numpy.array([1], numpy.int32) # values defining mask
        regrid = ESMP.ESMP_FieldRegridStore(srcFld,
                                            dstFld,
                                            srcMaskValues=maskVals,
                                            dstMaskValues=maskVals,
                                            regridmethod=ESMP.ESMP_REGRIDMETHOD_CONSERVE,
                                            unmappedaction=ESMP.ESMP_UNMAPPEDACTION_IGNORE,
                                            srcFracField=srcFracField,
                                            dstFracField=dstFracField)

        ESMP.ESMP_FieldRegrid(srcFld, dstFld, regrid)

        #
        # 6. check
        #

        ESMP.ESMP_FieldRegridGetArea(srcAreaField)
        ESMP.ESMP_FieldRegridGetArea(dstAreaField)
        srcAreasPtr = ESMP.ESMP_FieldGetPtr(srcAreaField)
        dstAreasPtr = ESMP.ESMP_FieldGetPtr(dstAreaField)

        srcFracPtr = ESMP.ESMP_FieldGetPtr(srcFracField)
        dstFracPtr = ESMP.ESMP_FieldGetPtr(dstFracField)

        # conservation
        srcFldSum, dstFldSum = srcFieldPtr.sum(), dstFieldPtr.sum()
        srcFldIntegral = (srcFieldPtr * srcAreasPtr * srcFracPtr).sum()
        dstFldIntegral = (dstFieldPtr * dstAreasPtr * dstFracPtr).sum()
        lackConservLocal = srcFldIntegral - dstFldIntegral

        # nans
        if numpy.isnan(srcFracPtr).sum() > 0:
            print '[%d] *** %d Nans found in srcFracPtr!!' % \
                (self.pe, numpy.isnan().sum(srcFracPtr))
        if numpy.isnan(dstFracPtr).sum() > 0:
            print '[%d] *** %d Nans found in dstFracPtr!!' % \
                (self.pe, numpy.isnan().sum(dstFracPtr))

        print '[%d] checksum of src: %g checksum of dst: %g' % \
            (self.pe, srcFldSum, dstFldSum)
        print '[%d] src total area integral: %g dst total area integral: %g diff: %g\n' % \
            (self.pe, srcFldIntegral, dstFldIntegral, lackConservLocal)

        lackConserv = MPI.COMM_WORLD.reduce(lackConservLocal,
                                            op=MPI.SUM, root=0)

        if self.pe == 0:
            print 'ROOT: total lack of conservation (should be small): %f' % lackConserv

        #
        # 7. clean up
        #

        ESMP.ESMP_FieldRegridRelease(regrid)
        ESMP.ESMP_FieldDestroy(srcAreaField)
        ESMP.ESMP_FieldDestroy(dstAreaField)
        ESMP.ESMP_FieldDestroy(srcFracField)
        ESMP.ESMP_FieldDestroy(dstFracField)
        ESMP.ESMP_FieldDestroy(srcFld)
        ESMP.ESMP_FieldDestroy(dstFld)
        ESMP.ESMP_GridDestroy(srcGrid)
        ESMP.ESMP_GridDestroy(dstGrid)

    def test1_ESMFRegrid(self):
        if self.pe == 0: print
        #
        # 1. input
        #

        coordSys = ESMP.ESMP_COORDSYS_SPH_DEG # ESMP.ESMP_COORDSYS_CART fails

        inFile = sys.prefix + \
            '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc'
        # dtype of a numpy 'float64' in scipy is '>f4' ...
        # switching to cdms2
        startBegin = time.time()
        srcF = cdms2.open(inFile)

        # Source Grid
        missing_value = 1.e20
        srcGrd = [srcF.variables['lat'], srcF.variables['lon']]
        srcBounds = _getCorners([srcF.variables['lat_vertices'],
                                 srcF.variables['lon_vertices']])
        srcGrdMask = srcF['so'][0, 0, ...].mask

        # Destination Grid
        lat1dBounds = numpy.arange(-90.0, 90.001, 5.0)
        lon1dBounds = numpy.arange(-180.0, 180.001, 5.0)
        lat1dCenter = 0.5*(lat1dBounds[:-1] + lat1dBounds[1:])
        lon1dCenter = 0.5*(lon1dBounds[:-1] + lon1dBounds[1:])
        lat2dBounds = numpy.outer(lat1dBounds,
                                  numpy.ones((len(lon1dBounds),),
                                               lon1dBounds.dtype ))
        lon2dBounds = numpy.outer(numpy.ones((len(lat1dBounds),),
                                              lat1dBounds.dtype ), lon1dBounds )
        lat2dCenter = numpy.outer(lat1dCenter,
                                  numpy.ones((len(lon1dCenter),),
                                               lon1dCenter.dtype ))
        lon2dCenter = numpy.outer(numpy.ones((len(lat1dCenter),),
                                              lat1dCenter.dtype ), lon1dCenter )
        dstGrd = [lat2dCenter, lon2dCenter]
        dstBounds = [lat2dBounds, lon2dBounds]

        #
        # 4. Regrid
        #
        srcGrdShape = srcGrd[0][:].shape
        dstGrdShape = dstGrd[0][:].shape
        startRegrid = time.time()
        ro = ESMFRegrid(srcGrdShape, dstGrdShape, numpy.float32,
                        'conserve', 'center', 0, 'degrees',
                        srcGridMask = srcGrdMask,
                        hasSrcBounds = True, 
                        hasDstBounds = True)

        # Get the slab and local part of the grid and bounds
        dstSlabCtr = ro.getDstLocalSlab("center")
        srcSlabCtr = ro.getSrcLocalSlab("center")

        # 
        srcGrdLcl = [coord[srcSlabCtr].data for coord in srcGrd]
        dstGrdLcl = [coord[dstSlabCtr] for coord in dstGrd]
        dstSlabCnr = ro.getDstLocalSlab("corner")
        srcSlabCnr = ro.getSrcLocalSlab("corner")
        srcBndLcl = [bound[srcSlabCnr] for bound in srcBounds]
        dstBndLcl = [bound[dstSlabCnr] for bound in dstBounds]

        ro.setCoords(srcGrdLcl, dstGrdLcl,
                     srcGridMask = srcGrdMask,
                     srcBounds = srcBndLcl, dstBounds = dstBndLcl,
                     globalIndexing = False)

        ro.computeWeights()

        # Local dimensions
        localDstData = numpy.ones(dstGrd[0].shape, numpy.float64)[dstSlabCtr[0], 
                                                                  dstSlabCtr[1]]
        localSrcData = srcF.variables['so'][0, 0, srcSlabCtr[0], srcSlabCtr[1]].data
        ro.apply(localSrcData, localDstData, rootPe = None, globalIndexing = False)
        endRegrid = time.time()

        #
        # 5. Check
        #

        # conservation
        diag = {'srcAreaFractions':None, 'srcAreas':None, 
                'dstAreaFractions':None, 'dstAreas':None}
        ro.fillInDiagnosticData(diag, rootPe = None)
        localSrcAreaFractions = diag['srcAreaFractions']
        localSrcAreas = diag['srcAreas']
        localDstAreaFractions = diag['dstAreaFractions']
        localDstAreas = diag['dstAreas']

        localSrcMass = (localSrcData * localSrcAreas * localSrcAreaFractions).sum()
        localDstMass = (localDstData * localDstAreas).sum()
        localLackConserve = localSrcMass - localDstMass

        # nans
        if numpy.isnan(localSrcAreaFractions).sum() > 0:
            a = self.pe
            b = numpy.isnan(localSrcAreaFractions).sum()
            #print '[%d] *** %d Nans found in localSrcAreaFractions!!' % (a, b)
#                (self.pe, str(numpy.isnan().sum(localSrcAreaFractions)))
        if numpy.isnan(localDstAreaFractions).sum() > 0:
            c = self.pe
            d = numpy.isnan(localDstAreaFractions).sum()
            #print '[%d] *** %d Nans found in localDstAreaFractions!!' % (c, d)
#                (self.pe, numpy.isnan().sum(localDstAreaFractions))

        #print '[%d] checksum of localSrc: %g checksum of localDst: %g' % \
            (self.pe, localSrcMass, localDstMass)
        #print '[%d] localSrc total area integral: %g localDst total area integral: %g diff: %g\n' % \
            (self.pe, localSrcMass, localDstMass, localLackConserve)

        lackConserv = MPI.COMM_WORLD.reduce(localLackConserve,
                                            op=MPI.SUM, root=0)

        if self.pe == 0:
            #print 'ROOT: total lack of conservation (should be small): %f' % lackConserv
            print 'Time for total including file opens:', time.time() - startBegin
            print 'Time for regridding ops:', endRegrid - startRegrid

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    ESMP.ESMP_LogSet(True)
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


