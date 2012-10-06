"""
Macro regridding class
David Kindig and Alex Pletzer, Tech-X Corp. (2012)
This code is provided with the hope that it will be useful.
No guarantee is provided whatsoever. Use at your own risk.
"""
import types
import re
import numpy
import cdms2
from error import CDMSError
import regrid2

def _areAreasOk(cornerCoords):
    """
    Check cell corner points (in 2D)
    @param cornerCoords
    @return None if OK, otherwise return a dict containing some diagnostics

        3
    +-------+
    | \     |
    |  \    |
  2 |   \   |4
    |    \  |
    |     \ |
    +-------+
        1
    """

    if len(cornerCoords) != 2:
        return True # no-op, no check

    dy1 = cornerCoords[0][ :-1, 1: ] - cornerCoords[0][ :-1,  :-1]
    dy2 = cornerCoords[0][1:  , :-1] - cornerCoords[0][ :-1,  :-1]
    dy3 = cornerCoords[0][1:  , :-1] - cornerCoords[0][1:  , 1:  ]
    dy4 = cornerCoords[0][ :-1, 1: ] - cornerCoords[0][1:  , 1:  ]

    dx1 = cornerCoords[1][ :-1, 1: ] - cornerCoords[1][ :-1,  :-1]
    dx2 = cornerCoords[1][1:  , :-1] - cornerCoords[1][ :-1,  :-1]
    dx3 = cornerCoords[1][1:  , :-1] - cornerCoords[1][1:  , 1:  ]
    dx4 = cornerCoords[1][ :-1, 1: ] - cornerCoords[1][1:  , 1:  ]

    areas = (dx1*dy2 - dx2*dy1 + dx3*dy4 - dx4*dy3)/2.0
    minAreas = areas.min()
    maxAreas = areas.max()

    if minAreas*maxAreas > 0:
        return None
    else:
        # something wrong
        return {'minAreas': minAreas, 
                'maxAreas': maxAreas, 
                'minAreasIndex': areas.argmin(), 
                'maxAreasIndex': areas.argmax(),
                'numZeroAreas': (areas == 0).sum(),
                'numNegativeAreas': (areas < 0).sum(),
                'numPositiveAreas': (areas > 0).sum(),
                'numCells': len(areas.flat),
                }

def _buildBounds(bounds):
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

def getBoundList(coordList, checkBounds=False):
    """
    Return a list of bounds built from a list of coordinates
    @param coordList coordinate list, should have getBounds()
    @param checkBounds set to True if you want to check bounds
    @return [latBounds, lonBounds]
    """
    cornerCoords = []
    for c in coordList:
        cornerC = _buildBounds(c.getBounds()[:])
        cornerCoords.append(cornerC)
        
    if checkBounds:
        res = _areAreasOk(cornerCoords)
        if res:
            raise CDMSError, \
                """Non-monotonic areas, check bounds
                   min cell area %(minAreas)g at index %(minAreasIndex)d
                   max cell area %(maxAreas)g at index %(maxAreasIndex)d
                   number of cells with zero areas %(numZeroAreas)d 
                   number of cells with negative areas %(numNegativeAreas)d 
                   number of cells with positive areas %(numPositiveAreas)d
                   total number of cells %(numCells)d
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
            srcBounds = getBoundList(srcCoords, 
                                     args.get('checkSrcBounds', False))
            dstBounds = getBoundList(dstCoords, 
                                     args.get('checkDstBounds', False))

            for c, b in zip(srcBounds, srcCoords):
                if c.min() == b.min() or c.max() == b.max():
                    print """   WARNING: Edge bounds are the same. The results
              of conservative regridding are not conserved.
              coordMin = %7.2f, boundMin = %7.2f, coordMax = %7.2f, boundMax = %7.2f
              """ % (c.min(), b.min(), c.max(), b.max())
            if srcBounds[0].min() < -90 or srcBounds[0].max() > 90 or \
               dstBounds[0].min() < -90 or dstBounds[0].max() > 90:
                raise CDMSError, """Bounds exceed +/-90 degree latitude"""
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

