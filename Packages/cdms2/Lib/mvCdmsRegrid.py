"""
Cdms2 interface to multiple regridders

David Kindig and Alex Pletzer, Tech-X Corp. (2012)
This code is provided with the hope that it will be useful.
No guarantee is provided whatsoever. Use at your own risk.
"""
import types
import operator
import re
import numpy
import cdms2
from error import CDMSError
import regrid2

def _areCellsOk(cornerCoords, mask=None):
    """
    Check cell corner points (in 2D)
    @param cornerCoords
    @param mask checks will not be performed where mask is 1 (True)
    @return None if OK, otherwise return a dict containing some diagnostics

   3         2
    +-------+ 
    | \     |
    |  \    |
    |   \   |
    |    \  |
    |     \ |
    +-------+
   0         1

    @note assumes cornerCoords are in lat, lon order
    """

    if len(cornerCoords) != 2:
        return True # no-op, no check

    def projectToSphere(the, lam):
        """ @return x, y, z coordinates in Cartesian space"""
        ct = numpy.cos(the)
        return ct*numpy.cos(lam), ct*numpy.sin(lam), numpy.sin(the)

    # compute area elements in Cartesian space
    lat0 = numpy.array(cornerCoords[0][ :-1,  :-1], numpy.float64)
    lat1 = numpy.array(cornerCoords[0][ :-1, 1:  ], numpy.float64)
    lat2 = numpy.array(cornerCoords[0][1:  , 1:  ], numpy.float64)
    lat3 = numpy.array(cornerCoords[0][1:  ,  :-1], numpy.float64)

    the0 = lat0*numpy.pi/180.
    the1 = lat1*numpy.pi/180.
    the2 = lat2*numpy.pi/180.
    the3 = lat3*numpy.pi/180.
    lam0 = numpy.array(cornerCoords[1][ :-1,  :-1], numpy.float64)*numpy.pi/180.
    lam1 = numpy.array(cornerCoords[1][ :-1, 1:  ], numpy.float64)*numpy.pi/180.
    lam2 = numpy.array(cornerCoords[1][1:  , 1:  ], numpy.float64)*numpy.pi/180.
    lam3 = numpy.array(cornerCoords[1][1:  ,  :-1], numpy.float64)*numpy.pi/180.
    
    x0, y0, z0 = projectToSphere(the0, lam0)
    x1, y1, z1 = projectToSphere(the1, lam1)
    x2, y2, z2 = projectToSphere(the2, lam2)
    x3, y3, z3 = projectToSphere(the3, lam3)

    dx10 = x1 - x0
    dy10 = y1 - y0
    dz10 = z1 - z0
    dx30 = x3 - x0
    dy30 = y3 - y0
    dz30 = z3 - z0
    dx32 = x3 - x2
    dy32 = y3 - y2
    dz32 = z3 - z2
    dx12 = x1 - x2
    dy12 = y1 - y2
    dz12 = z1 - z2
    dx20 = x2 - x0
    dy20 = y2 - y0
    dz20 = z2 - z0

    areas013 = [ (dy10*dz30 - dy30*dz10),
                 (dz10*dx30 - dz30*dx10),
                 (dx10*dy30 - dx30*dy10), ]
    areas231 = [ (dy32*dz12 - dy12*dz32),
                 (dz32*dx12 - dz12*dx32),
                 (dx32*dy12 - dx12*dy32), ]
    areas012 = [ (dy10*dz20 - dy20*dz10),
                 (dz10*dx20 - dz20*dx10),
                 (dx10*dy20 - dx20*dy10), ] 
    areas230 = [ (-dy32*dz20 + dy20*dz32),
                 (-dz32*dx20 + dz20*dx32),
                 (-dx32*dy20 + dx20*dy32), ]

    areas013Abs = numpy.sqrt( reduce(operator.add, 
                                     [areas013[i]**2 for i in range(3)]) )
    areas231Abs = numpy.sqrt( reduce(operator.add, 
                                     [areas231[i]**2 for i in range(3)]) )
    areas012Abs = numpy.sqrt( reduce(operator.add, 
                                     [areas012[i]**2 for i in range(3)]) )
    areas230Abs = numpy.sqrt( reduce(operator.add, 
                                     [areas230[i]**2 for i in range(3)]) )

    areas013DotAreas231 = reduce(operator.add, 
                                 [areas013[i]*areas231[i] for i in range(3)])
    areas012DotAreas230 = reduce(operator.add, 
                                 [areas012[i]*areas230[i] for i in range(3)])
    
    areasCriss = areas013Abs + areas231Abs
    areasCross = areas012Abs + areas230Abs

    minArea = 1.e-6 * numpy.pi * 2*numpy.pi / \
        float(areasCross.shape[0]*areasCross.shape[1])

    # Check that the cell has some area and check the 
    # topology

    bad = (areasCriss < minArea) | \
        (areasCross < minArea) | \
        (areas013DotAreas231 < 0.0) | \
        (areas012DotAreas230 < 0.0) | \
        (lat0 > 90.) | (lat1 > 90.) | (lat2 > 90.) |  (lat3 > 90.) | \
        (lat0 <-90.) | (lat1 <-90.) | (lat2 <-90.) |  (lat3 <-90.)

    if mask is not None:
        # exclude masked points
        bad *= (mask == 0)

    # inds contains list of bad cell indices
    inds = numpy.where(bad)
    
    if len(inds[0]) > 0:
        # package the result
        badCellIndices = [(inds[0][i], inds[1][i]) for i in range(len(inds[0]))]
        bcis1 = [(inds[0][i]  , inds[1][i]+1) for i in range(len(inds[0]))]
        bcis2 = [(inds[0][i]+1, inds[1][i]+1) for i in range(len(inds[0]))]
        bcis3 = [(inds[0][i]+1, inds[1][i]  ) for i in range(len(inds[0]))]
        badCellCoords = [[(cornerCoords[0][badCellIndices[i]], cornerCoords[1][badCellIndices[i]]),
                          (cornerCoords[0][bcis1[i]], cornerCoords[1][bcis1[i]]),
                          (cornerCoords[0][bcis2[i]], cornerCoords[1][bcis2[i]]),
                          (cornerCoords[0][bcis3[i]], cornerCoords[1][bcis3[i]])] \
                             for i in range(len(badCellIndices))]
        # problems...
        return {'numCells': len(areasCross.flat),
                'numBadCells': len(inds[0]),
                'badCellIndices': badCellIndices,
                'badCellCoords': badCellCoords,
                 }
    else:
        # everything is fine
        return None

def _buildBounds(bounds):
    """
    Build corner coordinates from bounds array
    @param bounds CdmsVar.getBounds()
    @return ndarrray of corners
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

def getBoundList(coordList, mask=None, 
                 removeBadCells=False, badCellIndices=[]):
    """
    Return a list of bounds built from a list of coordinates
    @param coordList coordinate list, should have getBounds()
    @param mask avoid checking areas where mask is one
    @param removeBadCells set to True if you want to the code to remove  
                bad cells, ie zero cells, butterfly cells, ...
    @param maskCellIndices list of bad cell indices to mask out (output)
    @return [latBounds, lonBounds]
    """
    cornerCoords = []
    for c in coordList:
        cornerC = _buildBounds(c.getBounds()[:])
        cornerCoords.append(cornerC)
        
    if removeBadCells:
        res = _areCellsOk(cornerCoords, mask=mask)
        if res:
            badCellIndices += res['badCellIndices']
            print """
-----------
WARNING: bad cell were detected
-----------
total number of cells: %(numCells)d
number of bad cells:   %(numBadCells)d

indices of bad cells:  
                      %(badCellIndices)s

bad cell coordinates:
                      %(badCellCoords)s
                """ % res

    return cornerCoords

def _getCoordList(grid):
    """
    Return a CDMS coordinate list from a CDMS grid
    @return lats, lons
    """

    lats = grid.getLatitude()
    lons = grid.getLongitude()

    if len(lats.shape) == 1 or len(lons.shape) == 1:

        # have axes, need to convert to curvilinear grid
        cgrid = grid.toCurveGrid()

        lats = cgrid.getLatitude()
        lons = cgrid.getLongitude()

    # we always want the coordinates in that order, these must
    # be cdms2 coordinates so we can inquire about bounds
    return lats, lons

def _getDstDataShape(srcVar, dstGrid):
    """
    Get the shape of the dst data
    @param srcVar the variable from which all axes other than lat/lon
                  will be taken from
    @param dstGrid target, horizontal grid
    @return list
    """

    shp = srcVar.shape
    ndims = len(shp)
    order = srcVar.getOrder()
    numX = order.count('x')
    numY = order.count('y')
    hasXY = (numX == 1) and (numY == 1)

    # fill in the axis list backwards, we're assuming the
    # y and x axes are more likely to occur at the end
    dstDataShape = []
    found = False
    j = 2
    for i in range(ndims-1, -1, -1):
        o = order[i]
        if not found and (o in 'xy') or (not hasXY and o == '-'):
            # add size from dst grid
            j -= 1
            dstDataShape = [dstGrid.shape[j],] + dstDataShape
            if j == 0:
                found = True
        else:
            # add size from src variable
            dstDataShape = [srcVar.shape[i],] + dstDataShape

    return dstDataShape

def _getAxisList(srcVar, dstGrid):
    """
    Get the list of axes from a variable and a grid
    @param srcVar the variable from which all axes other than lat/lon
                  will be taken from
    @param dstGrid target, horizontal grid
    @return variable with non-horizontal axes from srcVar and horizontal axes
            from dstGrid
    """

    shp = srcVar.shape
    ndims = len(shp)
    order = srcVar.getOrder()
    numX = order.count('x')
    numY = order.count('y')
    hasXY = (numX == 1) and (numY == 1)

    # fill in the axis list backwards, we're assuming the
    # y and x axes are more likely to occur at the end
    axisList = []
    found = False
    j = 2
    for i in range(ndims-1, -1, -1):
        o = order[i]
        if not found and (o in 'xy') or (not hasXY and o == '-'):
            # add axis from dst grid
            j -= 1
            axisList = [dstGrid.getAxis(j),] + axisList
            if j == 0:
                found = True
        else:
            # add axis from src variable
            axisList = [srcVar.getAxis(i),] + axisList

    return axisList

class CdmsRegrid:
    """
    Regridding switchboard, handles CDMS variables before handing off to
    regridder. If a multidimensional variable is passed in, the apply step
    loops over the axes above the Lat (Y) -- Lon (X) coordinates
    """
    def __init__(self, srcGrid, dstGrid, dtype,
                 regridMethod = 'linear', regridTool = 'libCF',
                 srcGridMask = None, srcGridAreas = None,
                 dstGridMask = None, dstGridAreas = None,
                 **args):
        """
        Establish which regridding method to use, handle CDMS variables before
        handing off to regridder. See specific tool for more information.

        @param srcGrid CDMS source grid
        @param dstGrid CDMS destination grid
        @param dtype numpy data type for src and dst data
        @param regridMethod linear (all tools - bi, tri),
                            conserve (ESMF Only)
                            patch (ESMF Only)
        @param regridTool LibCF, ESMF, ...
        @param srcGridMask array source mask, interpolation
                           coefficients will not be computed for masked
                           points/cells.
        @param srcGridAreas array destination cell areas, only needed for
                            conservative regridding
        @param dstGridMask array destination mask, interpolation
                           coefficients will not be computed for masked
                           points/cells.
        @param dstGridAreas array destination cell areas, only needed for
                            conservative regridding
        @param **args additional, tool dependent arguments
        """

        srcBounds = None
        dstBounds = None

        self.srcGrid = srcGrid
        self.dstGrid = dstGrid

        srcCoords = _getCoordList(srcGrid)
        dstCoords = _getCoordList(dstGrid)

        # retrieve and build a bounds list for conservative from the grids
        # We can't use the coords lists because if they are converted to
        # curvilinear
        # Set the tool to esmf if conservative selected. This overrides the
        # regridTool selection
        self.regridMethod = regridMethod
        if re.search( 'conserv', regridMethod.lower()):
            srcBadCellIndices = []
            srcBounds = getBoundList(srcCoords, srcGridMask,
                                     args.get('fixSrcBounds', False),
                                     badCellIndices = srcBadCellIndices)
            # mask out the bad src cells
            if len(srcBadCellIndices) > 0:
                if srcGridMask is None:
                    srcGridMask = numpy.zeros(srcCoords[0].shape, numpy.bool)
                for inds in srcBadCellIndices:
                    srcGridMask[inds] = 1 # True mean invalid      
            dstBadCellIndices = []
            dstBounds = getBoundList(dstCoords, dstGridMask,
                                     args.get('fixDstBounds', False),
                                     badCellIndices = dstBadCellIndices)
            # mask out the bad dst cells
            if len(dstBadCellIndices) > 0:
                if dstGridMask is None:
                    dstGridMask = numpy.zeros(dstCoords[0].shape, numpy.bool)
                for inds in dstBadCellIndices:
                    dstGridMask[inds] = 1 # True means invalid 
                    
            for c, b in zip(srcBounds, srcCoords):
                if c.min() == b.min() or c.max() == b.max():
                    print """   
WARNING: Edge bounds are the same. The results of conservative regridding will not conserve.
coordMin = %7.2f, boundMin = %7.2f, coordMax = %7.2f, boundMax = %7.2f
              """ % (c.min(), b.min(), c.max(), b.max())
            if srcBounds[0].min() < -90 or srcBounds[0].max() > 90 or \
               dstBounds[0].min() < -90 or dstBounds[0].max() > 90:
                print "WARNING: Bounds exceed +/-90 degree latitude: min/max lats = %g/%g" % \
                     (srcBounds[0].min(), srcBounds[0].max())
            if not re.search('esmp', regridTool.lower()):
                regridTool = 'esmf'

        # If LibCF handleCut is True, the bounds are needed to extend the grid
        # close the cut at the top
        if re.search('LibCF', regridTool, re.I) and args.has_key('handleCut'):
            if args['handleCut']: srcBounds = getBoundList(srcCoords)

        srcCoordsArrays = [numpy.array(sc) for sc in srcCoords]
        dstCoordsArrays = [numpy.array(dc) for dc in dstCoords]

        self.regridObj = regrid2.GenericRegrid(srcCoordsArrays, dstCoordsArrays,
                                               regridMethod = regridMethod,
                                               regridTool = regridTool,
                                               dtype = dtype,
                                               srcGridMask = srcGridMask,
                                               srcBounds = srcBounds,
                                               srcGridAreas = srcGridAreas,
                                               dstGridMask = dstGridMask,
                                               dstBounds = dstBounds,
                                               dstGridAreas = dstGridAreas,
                                               **args )
        self.regridObj.computeWeights(**args)

    def __call__(self, srcVar, **args):
        """
        Interpolate, looping over additional (non-latitude/longitude) axes
           if need be
        @param srcVar CDMS variable
        @param **args Tool dependent arguments
        @return CDMS interpolated variable
        """

        # initialize
        dstMask = None
        missingValue = getattr(srcVar, 'missing_value', None)

        timeAxis = srcVar.getTime()
        levelAxis = srcVar.getLevel()

        # shape of dst var
        dstShape = _getDstDataShape(srcVar, self.dstGrid)

        # establish the destination data. Initialize to missing values or 0.
        dstData = numpy.ones(dstShape, dtype = srcVar.dtype)
        if missingValue is not None and \
                re.search('conserv', self.regridMethod) is None:
            dstData *= missingValue
        else:
            dstData *= 0.0

        # interpolate the data, MPI gather on processor 0
        self.regridObj.apply(srcVar.data, dstData,
                             rootPe = 0,
                             missingValue = missingValue,
                             **args)

        # fill in diagnostic data
        if args.has_key('diag'):
            self.regridObj.fillInDiagnosticData(diag = args['diag'], rootPe = 0)

        # construct the axis list for dstVar
        dstAxisList = _getAxisList(srcVar, self.dstGrid)

        # harvest all the string attributes from srcVar
        attrs = {}
        for a in srcVar.attributes:
            v = srcVar.attributes[a]
            if type(v) is types.StringType:
                attrs[a] = v

        # if the missing value is present in the destination data, set
        # destination mask
        if numpy.any(dstData == missingValue):
            dstMask = (dstData == missingValue)

        # create the transient variable. Note: it is unclear whether
        # we should create the variable on the supplied dstGrid or
        # the local grid.
        dstVar = cdms2.createVariable(dstData,
                                      mask = dstMask,
                                      fill_value = missingValue,
                                      axes = dstAxisList,
                                      grid = self.dstGrid,
                                      attributes = attrs,
                                      id = srcVar.id + '_CdmsRegrid')

        return dstVar

