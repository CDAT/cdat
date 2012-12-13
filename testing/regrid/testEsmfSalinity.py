"""
$Id: testEsmfSalinity.py 2434 2012-08-22 03:46:36Z pletzer $

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
import sys

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
        
    def XXtest0_ESMP(self):
        
        import scipy.io.netcdf
        srcF = scipy.io.netcdf.netcdf_file('so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF.variables['so'][0, 0,...]
        missing_value = 1.e20
        srcGrd = [srcF.variables['lat'][:], srcF.variables['lon'][:]]
        srcBounds = _getCorners([srcF.variables['lat_vertices'][:], srcF.variables['lon_vertices'][:]])

        lat1dBounds = numpy.arange(-90.0, 90.001, 5.0)
        lon1dBounds = numpy.arange(-180.0, 180.001, 5.0)
        lat1dCenter = 0.5*(lat1dBounds[:-1] + lat1dBounds[1:])
        lon1dCenter = 0.5*(lon1dBounds[:-1] + lon1dBounds[1:])
        lat2dBounds = numpy.outer(lat1dBounds, numpy.ones( (len(lon1dBounds),), lon1dBounds.dtype ) )
        lon2dBounds = numpy.outer(numpy.ones( (len(lat1dBounds),), lat1dBounds.dtype ), lon1dBounds )
        lat2dCenter = numpy.outer(lat1dCenter, numpy.ones( (len(lon1dCenter),), lon1dCenter.dtype ) )
        lon2dCenter = numpy.outer(numpy.ones( (len(lat1dCenter),), lat1dCenter.dtype ), lon1dCenter )
        dstGrd = [lat2dCenter, lon2dCenter]
        dstBounds = [lat2dBounds, lon2dBounds]
        
        coordSys = ESMP.ESMP_COORDSYS_SPH_DEG # ESMP.ESMP_COORDSYS_CART fails
        
        srcDims = srcGrd[0].shape
        dstDims = dstGrd[0].shape

        # do we need to revert the order here?
        srcMaxIndex = numpy.array(srcDims[::-1], numpy.int32) # number of cells
        dstMaxIndex = numpy.array(dstDims[::-1], numpy.int32) # number of cells

        # grids
        srcGrid = ESMP.ESMP_GridCreateNoPeriDim(srcMaxIndex, 
                                        coordSys = coordSys)
        dstGrid = ESMP.ESMP_GridCreateNoPeriDim(dstMaxIndex, 
                                        coordSys = coordSys)

        # it's a good idea to always add the nodal coordinates
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


        # get pointer to coordinates array and dimensions
        # src
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

        # dst
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
        srcAreaField = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcAreas', 
                                                 typekind = ESMP.ESMP_TYPEKIND_R8,
                                                 staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstAreaField = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstAreas',
                                                 typekind = ESMP.ESMP_TYPEKIND_R8,
                                                 staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFracField = ESMP.ESMP_FieldCreateGrid(srcGrid, 'srcFracAreas',
                                                 typekind = ESMP.ESMP_TYPEKIND_R8,
                                                 staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFracField = ESMP.ESMP_FieldCreateGrid(dstGrid, 'dstFracAreas',
                                                 typekind = ESMP.ESMP_TYPEKIND_R8,
                                                 staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)

        srcFieldPtr = ESMP.ESMP_FieldGetPtr(srcFld)
        srcFracPtr = ESMP.ESMP_FieldGetPtr(srcFracField)
        dstFieldPtr = ESMP.ESMP_FieldGetPtr(dstFld)
        dstFracPtr = ESMP.ESMP_FieldGetPtr(dstFracField)

        # set the coordinates and field values. In ESMP, arrays are column major!

        # src-corners
        srcNx1 = srcHiCorner[0] - srcLoCorner[0]
        srcNy1 = srcHiCorner[1] - srcLoCorner[1]
        srcYCorner[:] = srcBounds[0][srcLoCorner[1]:srcHiCorner[1], srcLoCorner[0]:srcHiCorner[0]].flat
        srcXCorner[:] = srcBounds[1][srcLoCorner[1]:srcHiCorner[1], srcLoCorner[0]:srcHiCorner[0]].flat

        # src-center coordinates, field, and mask 
        srcNx = srcHiCenter[0] - srcLoCenter[0]
        srcNy = srcHiCenter[1] - srcLoCenter[1]
        yc = numpy.array(srcGrd[0][srcLoCenter[1]:srcHiCenter[1], srcLoCenter[0]:srcHiCenter[0]])
        xc = numpy.array(srcGrd[1][srcLoCenter[1]:srcHiCenter[1], srcLoCenter[0]:srcHiCenter[0]])
        srcYCenter[:] = yc.flat
        srcXCenter[:] = xc.flat
        msk = numpy.array(so[srcLoCenter[1]:srcHiCenter[1], srcLoCenter[0]:srcHiCenter[0]] \
                              == missing_value, numpy.int32)
        srcGridMaskPtr[:] = msk.flat
        fld = numpy.array(so[srcLoCenter[1]:srcHiCenter[1], srcLoCenter[0]:srcHiCenter[0]], so.dtype)
        # set to zero where masked
        fld *= (1 - msk)
        srcFieldPtr[:] = fld.flat
        srcFracPtr[:] = 1.0
        
        # dst-corners
        dstNx1 = dstHiCorner[0] - dstLoCorner[0]
        dstNy1 = dstHiCorner[1] - dstLoCorner[1]
        dstYCorner[:] = dstBounds[0][dstLoCorner[1]:dstHiCorner[1], dstLoCorner[0]:dstHiCorner[0]].flat
        dstXCorner[:] = dstBounds[1][dstLoCorner[1]:dstHiCorner[1], dstLoCorner[0]:dstHiCorner[0]].flat

        # dst-center coordinates, field, and mask 
        dstNx = dstHiCenter[0] - dstLoCenter[0]
        dstNy = dstHiCenter[1] - dstLoCenter[1]
        yc = numpy.array(dstGrd[0][dstLoCenter[1]:dstHiCenter[1], dstLoCenter[0]:dstHiCenter[0]])
        xc = numpy.array(dstGrd[1][dstLoCenter[1]:dstHiCenter[1], dstLoCenter[0]:dstHiCenter[0]])
        dstYCenter[:] = yc.flat
        dstXCenter[:] = xc.flat
        dstGridMaskPtr[:] = 0
        dstFieldPtr[:] = missing_value
        dstFracPtr[:] = 1.0

        # interpolation
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

        # get the cell areas
        ESMP.ESMP_FieldRegridGetArea(srcAreaField)
        ESMP.ESMP_FieldRegridGetArea(dstAreaField)
        srcAreasPtr = ESMP.ESMP_FieldGetPtr(srcAreaField)
        dstAreasPtr = ESMP.ESMP_FieldGetPtr(dstAreaField)

        srcFracPtr = ESMP.ESMP_FieldGetPtr(srcFracField)
        dstFracPtr = ESMP.ESMP_FieldGetPtr(dstFracField)

        # check conservation
        srcFldSum, dstFldSum = srcFieldPtr.sum(), dstFieldPtr.sum()
        srcFldIntegral = (srcFieldPtr * srcAreasPtr * srcFracPtr).sum()
        dstFldIntegral = (dstFieldPtr * dstAreasPtr * dstFracPtr).sum()
        lackConservLocal = srcFldIntegral - dstFldIntegral

        # check for nans
        if numpy.isnan(srcFracPtr).sum() > 0:
            print '[%d] *** %d Nans found in srcFracPtr!!' % (self.pe, numpy.isnan().sum(srcFracPtr))
        if numpy.isnan(dstFracPtr).sum() > 0:
            print '[%d] *** %d Nans found in dstFracPtr!!' % (self.pe, numpy.isnan().sum(dstFracPtr))
                                                                              
        print '[%d] checksum of src: %g checksum of dst: %g' % (self.pe, srcFldSum, dstFldSum)
        print '[%d] src total area integral: %g dst total area integral: %g diff: %g\n' % \
            (self.pe, srcFldIntegral, dstFldIntegral, lackConservLocal)

        lackConserv = MPI.COMM_WORLD.reduce(lackConservLocal, op=MPI.SUM, root=0)
        
        if self.pe == 0:
            print 'ROOT: total lack of conservation (should be small): %f' % lackConserv

        # cleanup
        ESMP.ESMP_FieldRegridRelease(regrid)
        ESMP.ESMP_FieldDestroy(srcAreaField)
        ESMP.ESMP_FieldDestroy(dstAreaField)
        ESMP.ESMP_FieldDestroy(srcFracField)
        ESMP.ESMP_FieldDestroy(dstFracField)
        ESMP.ESMP_FieldDestroy(srcFld)
        ESMP.ESMP_FieldDestroy(dstFld)
        ESMP.ESMP_GridDestroy(srcGrid)
        ESMP.ESMP_GridDestroy(dstGrid)


    def XXtest1_esmf(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        srcGridMask = numpy.array((so == so.missing_value), numpy.int32)
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0,...]
        srcGrd = [so.getGrid().getLatitude(), so.getGrid().getLongitude()]
        dG = clt.getGrid().toCurveGrid()
        dstGrd = [dG.getLatitude()[:], dG.getLongitude()[:]]

        srcBounds = cdms2.mvCdmsRegrid.getBoundList(srcGrd)
        dstBounds = cdms2.mvCdmsRegrid.getBoundList(dstGrd)
            

        srcGrid = esmf.EsmfStructGrid(srcGrd[0].shape, 
                            coordSys = ESMP.ESMP_COORDSYS_SPH_DEG)
        srcGrid.setCoords([numpy.array(coord) for coord in srcGrd], 
                          staggerloc = ESMP.ESMP_STAGGERLOC_CENTER,
                          globalIndexing = True)
        srcGrid.setCoords(srcBounds, 
                          staggerloc = ESMP.ESMP_STAGGERLOC_CORNER,
                          globalIndexing = True)
        srcGrid.setMask(srcGridMask)

        dstGrid = esmf.EsmfStructGrid(dstGrd[0].shape, 
                            coordSys = ESMP.ESMP_COORDSYS_SPH_DEG)
        dstGrid.setCoords([numpy.array(coord) for coord in dstGrd], 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER,
                          globalIndexing = True)
        dstGrid.setCoords(dstBounds, 
                          staggerloc = ESMP.ESMP_STAGGERLOC_CORNER,
                          globalIndexing = True)

        srcFld = esmf.EsmfStructField(srcGrid, 'srcField', 
                                      datatype = so.dtype, 
                                      staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFld.setLocalData(numpy.array(so), 
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER,
                            globalIndexing = True)
        dstFld = esmf.EsmfStructField(dstGrid, 'dstField', 
                                      datatype = so.dtype, 
                                      staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFld.setLocalData(numpy.array(clt) * 0,
                            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER,
                            globalIndexing = True)

        srcFracFld = esmf.EsmfStructField(srcGrid, 'srcFracAreasFld', 
                                          datatype = 'float64', 
                                          staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        srcFracFld.setLocalData(numpy.ones(srcGrd[0].shape, dtype=srcGrd[0].dtype),
                                staggerloc = ESMP.ESMP_STAGGERLOC_CENTER,
                                globalIndexing = True)
        dstFracFld = esmf.EsmfStructField(dstGrid, 'dstFracAreasFld', 
                                          datatype = 'float64', 
                                          staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFracFld.setLocalData(numpy.ones(dstGrd[0].shape, dtype=dstGrd[0].dtype),
                                staggerloc = ESMP.ESMP_STAGGERLOC_CENTER,
                                globalIndexing = True)

        # this fails on 7 and 8 procs (passing dstFrac = None works)
        ro = esmf.EsmfRegrid(srcFld, dstFld,
                             srcFrac = srcFracFld, dstFrac = None,
                             srcMaskValues = numpy.array([1], numpy.int32),
                             dstMaskValues = numpy.array([1], numpy.int32),
                             regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE,
                             unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE)
        # interpolate
        ro()
        dstData = dstFld.getData(rootPe = 0)
        if self.pe == 0:
            dstMask = (dstData >= 100 )
            dstDataM = numpy.ma.array(dstData, mask = dstMask)
            dstDataM.missing_value = so.missing_value

            zeroVal = (dstDataM == 0)

            dstDataMMin = dstDataM.min()
            dstDataMMax = dstDataM.max()
            print 'Number of zero valued cells', zeroVal.sum()
            print 'min/max value of dstDataM: %f %f' % (dstDataMMin, dstDataMMax)                               
            self.assertLess(dstDataMMax, so.max())

    def XXtest2_ESMFRegrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF['so']
        srcGridMask = numpy.array((so[0, 0,...] == so.missing_value) , numpy.int32)
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, ...]
        srcGrd = [so.getGrid().getLatitude(), so.getGrid().getLongitude()]
        srcBounds = cdms2.mvCdmsRegrid.getBoundList(srcGrd)
        dG = clt.getGrid().toCurveGrid()
        dstGrd = [dG.getLatitude()[:], dG.getLongitude()[:]]
        dstBounds = cdms2.mvCdmsRegrid.getBoundList(dstGrd)


        # create regrid object
        srcGrdShape = srcGrd[0].shape
        dstGrdShape = dstGrd[0].shape
        r = regrid2.mvESMFRegrid.ESMFRegrid(srcGrdShape, dstGrdShape, 
                                            dtype=so.dtype,
                                            regridMethod='conserve', 
                                            staggerLoc='center', 
                                            periodicity=0, 
                                            coordSys='deg',
                                            srcGridMask=srcGridMask, 
                                            hasSrcBounds = True,
                                            hasDstBounds = True)
        globalIndexing = False
        # Find the slabs for each processor
        dstSlab = r.getDstLocalSlab("center")
        srcSlab = r.getSrcLocalSlab("center")
        srcBSlab = r.getSrcLocalSlab("corner")
        dstBSlab = r.getDstLocalSlab("corner")

        r.setCoords([numpy.array(g[srcSlab]) for g in srcGrd], 
                    [numpy.array(g[dstSlab]) for g in dstGrd], 
                    srcGridMask=srcGridMask, 
                    srcBounds = [numpy.array(b[srcBSlab]) for b in srcBounds],
                    dstBounds = [numpy.array(b[dstBSlab]) for b in dstBounds], 
                    globalIndexing = globalIndexing)

        # compute weights
        start = time.time()
        r.computeWeights()
        finish = (time.time() - start)

        print '\nSeconds to computeWeights -> finish  = %f s' % numpy.round(finish,2)

        # Get the source data slab/ create dst data container
        localSrcData = so[0, 0, srcSlab[0], srcSlab[1]].data
        dstShp = dG.getLatitude().shape
        dstData = numpy.ones(dstShp, so.dtype)[dstSlab[0], dstSlab[1]] * so.missing_value

        # interpolate
        r.apply(localSrcData, dstData, rootPe = None, 
                globalIndexing = None)
        
        if self.pe == 0:
            # checks
            dstDataMask = (dstData == so.missing_value)
            print 'number of masked values = ', dstDataMask.sum()
            dstDataFltd = dstData * (1 - dstDataMask)
            if so.missing_value > 0:
                dstDataMin = dstData[:].min()
                dstDataMax = dstDataFltd.max()
            else:
                dstDataMin = dstDataFltd.min()
                dstDataMax = dstData[:].max()
            print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)                               
            self.assertLess(dstDataMax, so[0,0,...].max())

    def XXtest3_genericRegrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        srcGridMask = numpy.array((so == so.missing_value) , numpy.int32)
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        srcGrd = [so.getGrid().getLatitude(), so.getGrid().getLongitude()]
        srcBounds = cdms2.mvCdmsRegrid.getBoundList(srcGrd)
        dG = clt.getGrid().toCurveGrid()
        dstGrd = [dG.getLatitude(), dG.getLongitude()]
        dstBounds = cdms2.mvCdmsRegrid.getBoundList(dstGrd)
        # create regrid object
        r = regrid2.mvGenericRegrid.GenericRegrid([numpy.array(coord) for coord in srcGrd],
                                                  [numpy.array(coord) for coord in dstGrd],
                                                  dtype=so.dtype,
                                                  regridMethod='conserve', 
                                                  regridTool='esmf',
                                                  srcGridMask=srcGridMask, 
                                                  srcBounds=srcBounds, 
                                                  srcGridAreas=None,
                                                  dstGridMask=None, 
                                                  dstBounds=dstBounds, 
                                                  dstGridAreas=None)
        # compute weights
        r.computeWeights()
        # create dst data container
        dstShp = dG.getLatitude().shape
        dstData = numpy.ones(dstShp, so.dtype) * so.missing_value
        # interpolate
        r.apply(numpy.array(so), dstData, rootPe = 0)

        if self.pe == 0:
            # checks
            dstDataMask = (dstData == so.missing_value)
            print 'number of masked values = ', dstDataMask.sum()
            dstDataFltd = dstData * (1 - dstDataMask)
            if so.missing_value > 0:
                dstDataMin = dstData.min()
                dstDataMax = dstDataFltd.max()
            else:
                dstDataMin = dstDataFltd.min()
                dstDataMax = dstData.max()
            print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)                               
            self.assertLess(dstDataMax, so.max())

    def test4_cdmsRegrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        srcGridMask = numpy.array((so == so.missing_value) , numpy.int32)
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        # create regrid object
        r = cdms2.CdmsRegrid(so.getGrid(), clt.getGrid(),
                             dtype=so.dtype,
                             regridMethod='conserve', 
                             regridTool='esmf',
                             srcGridMask=srcGridMask, 
                             srcGridAreas=None,
                             dstGridMask=None, 
                             dstGridAreas=None)
        dstData = r(so)

        # checks
        if self.pe == 0:
            dstDataMask = (dstData == so.missing_value)
            print 'number of masked values = ', dstDataMask.sum()
            self.assertTrue(str(type(dstData)), str(type(clt)))
            dstData.mask = (dstData == so.missing_value)
            dstDataMin = dstData.min()
            dstDataMax = dstData.max()
            zeroValCnt = (dstData == 0).sum()
            print 'Number of zero valued cells', zeroValCnt
            print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)
            self.assertLess(dstDataMax, so.max())


    def XXtest5_regrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        dstData = so.regrid(clt.getGrid(), 
                            regridTool = 'esmf', 
                            regridMethod='conserve')

        if self.pe == 0:
            dstDataMask = (dstData == so.missing_value)
            dstDataFltd = dstData * (1 - dstDataMask)
            zeroValCnt = (dstData == 0).sum()
            if so.missing_value > 0:
                dstDataMin = dstData.min()
                dstDataMax = dstDataFltd.max()
            else:
                dstDataMin = dstDataFltd.min()
                dstDataMax = dstData.max()
                zeroValCnt = (dstData == 0).sum()
            print 'Number of zero valued cells', zeroValCnt
            print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)                   
            self.assertLess(dstDataMax, so.max())
            if False:
                pylab.figure(1)
                pylab.pcolor(so, vmin=20, vmax=40)
                pylab.colorbar()
                pylab.title('so')
                pylab.figure(2)
                pylab.pcolor(dstData, vmin=20, vmax=40)
                pylab.colorbar()
                pylab.title('dstData')

if __name__ == '__main__':
    print ""

    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


