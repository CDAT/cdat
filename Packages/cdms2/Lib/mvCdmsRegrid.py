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

   3         2
    +-------+ 
    | \     |
    |  \    |
    |   \   |
    |    \  |
    |     \ |
    +-------+
   0         1
    """

    if len(cornerCoords) != 2:
        return True # no-op, no check

    def projectToSphere(the, lam):
        """ @return x, y, z coordinates in Cartesian space"""
        ct = numpy.cos(the)
        return ct*numpy.cos(lam), ct*numpy.sin(lam), numpy.sin(the)

    # compute area elements in Cartesian space
    the0 = cornerCoords[0][ :-1,  :-1]*numpy.pi/180.
    the1 = cornerCoords[0][ :-1, 1:  ]*numpy.pi/180.
    the2 = cornerCoords[0][1:  , 1:  ]*numpy.pi/180.
    the3 = cornerCoords[0][1:  ,  :-1]*numpy.pi/180.
    lam0 = cornerCoords[1][ :-1,  :-1]*numpy.pi/180.
    lam1 = cornerCoords[1][ :-1, 1:  ]*numpy.pi/180.
    lam2 = cornerCoords[1][1:  , 1:  ]*numpy.pi/180.
    lam3 = cornerCoords[1][1:  ,  :-1]*numpy.pi/180.
    
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

    area012 = [dy10*dz30 - dy30*dy10,
               dz10*dx30 - dz30*dx10,
               dx10*dy30 - dx30*dy10]
    area231 = [dy32*dz12 - dy12*dy32,
               dz32*dx12 - dz12*dx32,
               dx32*dy12 - dx12*dy32]
    

    # the 2 triangle areas should be aligned
    area012dotArea231 = reduce(lambda x,y:x+y, [area012[i]*area231[i] for i in range(3)])
    
    if numpy.any(area012dotArea231 < 0.):
        # bad indexing?
        inds = numpy.where(area012dotArea231 < 0.)
        badCellIndices = [(inds[0][i], inds[1][i]) for i in range(len(inds[0]))]
        bcis1 = [(inds[0][i]  , inds[1][i]+1) for i in range(len(inds[0]))]
        bcis2 = [(inds[0][i]+1, inds[1][i]+1) for i in range(len(inds[0]))]
        bcis3 = [(inds[0][i]+1, inds[1][i]  ) for i in range(len(inds[0]))]
        # problems...
        return {'numCells': len(area012dotArea231.flat),
                'numBadCells': (area012dotArea231 < 0.).sum(),
                'badCellIndices': str(badCellIndices),
                'badCellSphericalAreas012': str([(area012[0][bci], area012[1][bci], area012[2][bci]) \
                                                     for bci in badCellIndices]),
                'badCellSphericalAreas231': str([(area231[0][bci], area231[1][bci], area231[2][bci]) \
                                                     for bci in badCellIndices]),
                'badCellCornerCoords0': str([(cornerCoords[1][bci], cornerCoords[0][bci]) \
                                                 for bci in badCellIndices]),
                'badCellCornerCoords1': str([(cornerCoords[1][bci], cornerCoords[0][bci]) \
                                                 for bci in bcis1]),
                'badCellCornerCoords2': str([(cornerCoords[1][bci], cornerCoords[0][bci]) \
                                                 for bci in bcis2]),
                'badCellCornerCoords3': str([(cornerCoords[1][bci], cornerCoords[0][bci]) \
                                                 for bci in bcis3]),
                }
    else:
        # everything is fine
        return None

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
                """Area checks
                   total number of cells %(numCells)d
                   number of bad cells %(numBadCells)d
                   indices of bad cells %(badCellIndices)s
                   bad cell coordinates:
                                        %(badCellCornerCoords0)s
                                        %(badCellCornerCoords1)s
                                        %(badCellCornerCoords2)s
                                        %(badCellCornerCoords3)s
                   bad cell areas:
                                  %(badCellSphericalAreas012)s
                                  %(badCellSphericalAreas231)s
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

