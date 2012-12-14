"""
Test conservative regridding of strange grids
CNRM grid

$Id: testCNRMGrid.py 2472 2012-10-12 13:10:52Z pletzer $
"""
import cdms2
import numpy
import ESMP
import glob
import os
import unittest

ESMP.ESMP_LogSet(True)

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

class TestGrid(unittest.TestCase):

  def setUp(self):
    filename = sys.prefix + \
        "/sample_data/so_Omon_CNRM-CM5_decadal2004_r9i1p1_200501-201412_2timesteps.nc"
    g = cdms2.open(filename)
    self.so = g('so')[0,0,...]
    self.so.toVisit('soCNRM.vsh5', 'Vs')
    gLat = cdms2.createGaussianAxis(64)
    deltaLon = (360/128.)
    gLon = cdms2.createUniformLongitudeAxis(0, 128, deltaLon)
    self.gaussGrid = cdms2.grid.createGenericGrid(gLat[:], gLon[:], 
                                         gLat.getBounds(),
                                         gLon.getBounds())

  def test1(self):
    soN = self.so.regrid(self.gaussGrid, rt = 'esmf', rm = 'conserve',
                         coordSys = 'deg', periodicity = 1, 
                         fixSrcBounds=True)
    self.so.toVisit('CNRM_so.vsh5','Vs')
    soN.toVisit('CNRM_so_interp.vsh5', 'Vs')

  def Xtest2(self):
    """
    Using the native ESMP interface
    """
    import scipy.io.netcdf
    filename = (sys.prefix + \
                "/sample_data/so_Omon_CNRM-CM5_decadal2004_r9i1p1_200501-201412_2timesteps.nc")
    srcF = scipy.io.netcdf.netcdf_file(filename)
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
    

if __name__ == '__main__':
    ESMP.ESMP_Initialize()
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(TestGrid)
    unittest.TextTestRunner(verbosity = 2).run(suite)
