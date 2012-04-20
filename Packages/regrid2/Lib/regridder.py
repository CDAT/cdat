import numpy, copy
import cdms2
import regrid2
from regrid2 import RegridError
import re

esmfImported = False
try:
    from regrid2 import esmf
    import ESMP
    esmfImported = True
except:
    pass

def _setMaskDtype(mask):
    """
    Convert a mask to int32
    @param mask the mask to be converted
    """
    if mask.dtype != numpy.int32:
        if mask.dtype == numpy.bool:
            mask = numpy.array(mask, numpy.int32)
        elif mask.dtypye == numpy.bool8:
            mask = numpy.array(mask, numpy.int32)

    return mask

def _makeGridList(grid):
    """
    Convert a cdms2 grid to a list of coordinates
    Using this because TransientRectGrids have no AxisList though the method
    exists
    @param grid The grid to be converted
    """
    if cdms2.isGrid(grid):
        # It is a grid
        index = 0
        retGrid = [grid.getLatitude(), grid.getLongitude()]
        try:
            retGrid.append(grid.getLevel())
        except:
            pass
        nSpatial = len(retGrid)
    elif isinstance(grid, list):
        # It is a list already
        nSpatial = len(grid)
        retGrid = grid
    else:
        raise RegridError, 'Grid must be a list of coordinates or a cdms2 grid'

    # Check the number of coordinates
    if nSpatial < 2:
        raise RegridError, 'Only one coordinate found. Regridding needs at\n' + \
                           'least two.'
    return retGrid, nSpatial

def hasMask(data):
    """
    Does the data provided contain a mask?
    @param data
    """
    hasMask = False
    if hasattr(data, 'mask'):
        if data.mask.size > 1:
            hasMask = True
    return hasMask

def _makeCrdsFromBounds(coords = None):
    """
    Need to build a mesh that is nodal for the ESMF regridding to work
    consevatively. The models use both 1d and 2d axes.
    @param list of coordinates [lon, lat]
    """
    if coords is None: 
        raise RegridError, 'Coordinates required'
    if not isinstance(coords, list):
        raise RegridError, 'Coordinates must be a list'

    rank = coords[0].shape
    bounds = []
    for c in coords:
        bounds.append(c.getBounds())

    if len(rank) == 1:

        # 1-d axes have different dimensions for each
        ni = len(coords[1])
        nj = len(coords[0])
        newDims = [nj+1, ni+1]
        newMeshLons = numpy.zeros(newDims[1], numpy.float64)
        newMeshLats = numpy.zeros(newDims[0], numpy.float64)

        newMeshLons[:ni] = bounds[1][:, 0]
        newMeshLons[-1] = bounds[1][ni-1, 1]
        newMeshLats[:nj] = bounds[0][:, 0]
        newMeshLats[-1] = bounds[0][nj-1, 1]
        if newMeshLats[0] < -90: newMeshLats[0] = -90
        if newMeshLats[-1] > 90: newMeshLats[-1] = 90
        gridDims = newDims

    elif len(rank) == 2:

        # 2-d axes have the same dimensions for each
        nj = rank[0]+1
        ni = rank[1]+1

        def checkBndOrder(bnd):
            if bnd[0] <= bnd[1] and bnd[1] >= bnd[2]:
                order = (0, 1, 2, 3)
            elif bnd[0] <= bnd[1] and bnd[1] <= bnd[2]:
                order = (0, 1, 2, 3)
            elif bnd[0] >= bnd[1] and bnd[1] <= bnd[2]:
                order = (2, 3, 0, 1)
            elif bnd[0] >= bnd[1] and bnd[1] >= bnd[2]:
                order = (3, 0, 1, 2)
            else:
                order = bnd
            return order

        newMeshLons = numpy.zeros((nj, ni), numpy.float64)
        newMeshLats = numpy.zeros((nj, ni), numpy.float64)
        mnj, mni, nnj, nni = nj-1, ni-1, nj-2, ni-2

        o = checkBndOrder(bounds[1][0, 0, :])

        newMeshLons[:mnj, :mni] = bounds[1][  :,   :, o[0]]   # Lower Left
        newMeshLats[:mnj, :mni] = bounds[0][  :,   :, o[0]]   # Lower Left
        newMeshLons[:mnj,   -1] = bounds[1][  :,  -1, o[1]]   # Right Edge
        newMeshLats[:mnj,   -1] = bounds[0][  :,  -1, o[1]]   # Right Edge
        newMeshLons[  -1, :mni] = bounds[1][ -1,   :, o[3]]   # Top Row
        newMeshLats[  -1, :mni] = bounds[0][ -1,   :, o[3]]   # Top Row
        newMeshLons[  -1,   -1] = bounds[1][ -1,  -1, o[2]]   # Upper Right corner
        newMeshLats[  -1,   -1] = bounds[0][ -1,  -1, o[2]]   # Upper Right corner

        gridDims = (nj, ni)

    else:
        raise RegridError, '3D+ interp not supported yet...'

    return [newMeshLons, newMeshLats], gridDims

def _makeBoundsCurveList(grid):
    """
    Construct a curvilinear grid from the bounds of the given grid
    @param grid the grid. Needs to have getBounds()
    """
    if isinstance(grid, list):
        for g in grid:
            if not hasattr(g, 'getBounds'):
                raise RegridError, 'cdms2 grid required in list'
        gridSave = grid
        # Create new grid and replace
        bounds, newDims = _makeCrdsFromBounds(grid)
    elif cdms2.isGrid(grid):
        g = [grid.getLongitude(), grid.getLatitude()]
        bounds, newDims = _makeCrdsFromBounds(g)
    else:
        message = "Input grid must be a list of grids"
        raise RegridError, message

    srcBoundsCurve = cdms2.gsRegrid.makeCurvilinear(bounds)

class Regridder:
    def __init__(self, inGrid, outGrid, srcMask = None, dstMask = None,
                 regridTool = "gsRegrid", regridMethod = "bilinear", **args):
        """
        Constructor for regridding object. Currently just uses a 2-D object
        @param ingrid cdms2, ndarray variable
        @param outGrid cdms2, ndarray variable
        @param srcMask mask for the source grid - use numpy masking rules
                        0 - Valid, 1 - Invalid
        @param dstMask mask for the destination grid
        @param regridTool Which regridder to use.
                        regrid2 - uses only axis CDAT original regriding tool
                        gsRegrid - curviliner grids - libcf is a synonym
                        esmf - curvilinear grids, axes, bilinear, conservative.
                               ESMP is a synonym.
                        scrip - many options but requires a remapping file from
                                SCRIP
        @param regridMethod Conservative, Mutlilinear. Some regrid tools use
                            only bilinear or multilinear
        @param **args Optional keyword arguments for each type of regridder
                gsRegrid accepts nitermax and tolpos for computing the weights
        """
        if regridTool is None:
            regridTool = 'gsregrid'
        if regridMethod is None:
            regridMethod = 'bilinear'
        rgTool = regridTool.lower()
        rgMeth = regridMethod.lower()
        self.regridTool = rgTool
        self.regridMethod = rgMeth
        self.outMask = None

        if re.match('regrid', rgTool, re.I):
            self.regridObj = regrid2.Horizontal(ingrid, outGrid)
            self.regridTool = 'regrid2'
        elif rgTool == 'scrip':
            pass
        elif re.match('esm', rgTool):
            if not esmfImported:
                string = "ESMP is not installed or is unable to be imported"
                raise RegridError, string
            self.esmfImported = esmfImported
            self.regridTool = 'esmp'

            # Set the regridding method - Bilinear / Conservative
            if regridMethod is None or regridMethod.lower() == 'bilinear':
                self.regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR
            else:
                self.regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE

            # Want 2D coordinates in a list [lat, lon]
            # Choices list, TransientAxis2D, TransientRectAxis
            srcGrid, self.srcSpatial = _makeGridList(inGrid)
            if self.srcSpatial > 1:
                srcGrid, srcSpatial = cdms2.gsRegrid.makeCurvilinear(srcGrid)

            dstGrid, self.dstSpatial = _makeGridList(outGrid)
            if self.dstSpatial > 1:
                dstGrid, dstSpatial = cdms2.gsRegrid.makeCurvilinear(dstGrid)

            self.location = ESMP.ESMP_STAGGERLOC_CENTER
            srcBoundsCurveList = None
            dstBoundsCurveList = None
            if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
                # Create new coordinates from bounds if available
                self.location = ESMP.ESMP_STAGGERLOC_CORNER
                srcBoundsCurveList = _makeBoundsCurveList(inGrid)
                dstBoundsCurveList = _makeBoundsCurveList(outGrid)

            # Set some required values
            self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_ERROR
            self.staggerloc = ESMP.ESMP_STAGGERLOC_CENTER
            self.coordSys = ESMP.ESMP_COORDSYS_CART
            self.periodicity = 1

            for arg in args:
                if re.search('unmappedaction', arg):
                    unMappedAction = args[arg]
                    if re.search('error', unMappedAction):
                        self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_ERROR
                if re.search('periodicity', arg):
                    self.periodicity = args[arg]
                if re.search('staggerloc', arg):
                    self.staggerloc = args[arg]
                if re.search('coordsys', arg):
                    self.coordSys = args[arg]

            # Initialize ESMP
            esmf.initialize()

            self.outMask = srcMask
            self.dstMask = dstMask

            # Create the ESMP grids
            self.srcGrid = esmf.EsmfStructGrid(srcGrid, 
                                               bounds = srcBoundsCurveList, 
                                               mask = srcMask,
                                               regridMethod = regridMethod, 
                                               periodicity = self.periodicity,
                                               staggerloc = self.staggerloc,
                                               coordSys = self.coordSys)
            self.dstGrid = esmf.EsmfStructGrid(dstGrid, 
                                               bounds = dstBoundsCurveList,
                                               mask = dstMask,
                                               regridMethod = regridMethod, 
                                               periodicity = self.periodicity,
                                               staggerloc = self.staggerloc,
                                               coordSys = self.coordSys)

        elif rgTool == 'gsregrid' or rgTool == 'libcf':
            # Prep in and out grid for gsRegrid!
            self.srcGrid, self.srcRank = _makeGridList(inGrid)
            self.dstGrid, self.dstRank = _makeGridList(outGrid)

            if self.dstRank != self.srcRank:
                raise RegridError, 'outGrid rank (%d) != inGrid rank (%d)' % \
                                   (self.dstRank, self.srcRank)

            # Create the regrid object
            ro = regrid2.gsRegrid.Regrid(self.srcGrid, self.dstGrid)

            # Set the source mask
            if srcMask is not None:
                if len(srcMask.shape) > 3:
                    raise RegridError, \
                           'Ranks greater than three are not supported'
                self.srcMask = _setMaskDtype(srcMask)
                ro.setMask(self.srcMask)

            # Compute the weights
            self.nitermax = 20
            self.nlat = self.dstGrid[0].shape[0]
            self.tolpos = 0.01 * 180. / (self.nlat-1)
            if 'nitermax' in args.keys():
                self.nitermax = args['nitermax']
            if 'tolpos' in args.keys():
                self.tolpos = args['tolpos']
            ro.computeWeights(nitermax = self.nitermax,
                              tolpos   = self.tolpos)
            self.regridTool = 'gsregrid'
            self.regridObj = ro
        else:
            raise RegridError, ''' Regrid tools are: regrid2,
                  (gsRegrid or libcf),
                  (esmf or esmp)'''

    def __call__(self, inData, **args):
        """
        Apply the interpolation.
        @param inData The input data
        @param missing Missing value
        @param order The order of the data
        @param mask Mask
        @param **args Optional keyword arguments for each regrid type
                regrid2 has: missing = None, order = None, mask = None,
        """

        from cdms2.avariable import AbstractVariable
        from cdms2.tvariable import TransientVariable
        if isinstance(inData, AbstractVariable):
            if not isinstance(inData, numpy.ndarray):
                inData = inData.data

            attrs = copy.copy(inData.attributes)
            varid = inData.id
            axisList = list(map(lambda x: x[0].clone(), inData.getDomain()))
            inputIsVariable = 1
            if 'order' not in args.keys():
                order = inData.getOrder()
            #this expects contiguous arrays
            if isinstance(inData, TransientVariable) and inData.iscontiguous() is False:
                inData = inData.ascontiguous()
        else:
            inputIsVariable = 0

        missing = None
        mvAttList = ['missing_value', 'fill_value']
        if 'missing' in args.keys():
            missing = args['missing']
        else:
            for att in mvAttList:
                if hasattr(inData, att):
                    missing = getattr(inData, att)

        if self.regridTool == 'regrid2':
            result = self.regridObj(inData, args)
        elif self.regridTool == 'esmp':
            if not self.esmfImported:
                string = "ESMP is not installed or is unable to be imported"
                raise RegridError, string

            if self.location is None:
                location = ESMP.ESMP_STAGGERLOC_CENTER
            else:
                location = self.location

            location = ESMP.ESMP_STAGGERLOC_CENTER

            # Make sure we are passing a ndarray
            self.srcField = esmf.EsmfGridField(self.srcGrid, inData.id,
                                                 inData.data,
                                                 staggerloc = location)
            # Convert bask y, x
            outShape = self.dstGrid.maxIndex[::-1]
            outVar = numpy.zeros(outShape, inData.dtype)
            self.dstField = esmf.EsmfGridField(self.dstGrid, inData.id,
                                               outVar,
                                               staggerloc = location)
            if self.regridMethod is not None:
                method = self.regridMethod
            else:
                method = ESMP.ESMP_REGRIDMETHOD_BILINEAR

            if self.unMappedAction is not None:
                unMappedAction = self.unMappedAction
            else:
                unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE

            self.regrid = esmf.EsmfRegrid(self.srcField, self.dstField, method,
                                          unMappedAction)
            # Call the regrid proceedure
            self.regrid()
            ptr = self.dstField.getPointer()
            outVar.flat = self.dstField.getPointer()

            # Set the output mask if available
            outMask = None
            if self.dstMask is not None:
                outMask = self.dstMask
            elif hasMask(inData) or self.outMask is not None:
                attrs = ('missing_value','fill_value',)
                missing = None
                for att in attrs:
                    if hasattr(inData, att):
                       missing = getattr(inData, att)
                if missing is not None: outMask = (outVar == missing)

            # Need to convert this to a cdms2 variable
            if inputIsVariable:
                # Grid
                attrs = inData.attributes
                lats = numpy.reshape(self.dstGrid.getPointer(1), outVar.shape)
                lons = numpy.reshape(self.dstGrid.getPointer(2), outVar.shape)
                lat = cdms2.coord.TransientAxis2D(lats, id = 'lat')
                lon = cdms2.coord.TransientAxis2D(lons, id = 'lon')
                grid = cdms2.hgrid.TransientCurveGrid(lat, lon, id = 'CurveGrid')
                result = cdms2.createVariable(outVar, mask = outMask,
                                              fill_value = inData.fill_value,
                                              axes = grid.getAxisList(),
                                              grid = grid,
                                              attributes = attrs, id = varid)
            else:
                result = numpy.ma.masked_array(outVar, mask = outMask, fill_value = missing)

            return result

        elif self.regridTool == 'scrip':
            pass
        elif self.regridTool == 'gsregrid':

            # The data has a mask and the mask has not been set previously
            # If the mask is then set, the weights must be computed...
            if hasMask(inData) and self.regridObj.maskSet is False:
                # Reset the weightsComputed flag since they will be recalculated
                self.regridObj.weightsComputed = False
                self.regridObj.setMask(inData.mask)
                # Recompute/Compute the weights taking the mask into account
                self.regridObj.computeWeights(tolpos = self.tolpos,
                                              nitermax = self.nitermax)

            # If the weights have somehow escaped being generated, compute them
            if not self.regridObj.weightsComputed:
                self.regridObj.computeWeights(tolpos = self.tolpos,
                                              nitermax = self.nitermax)

            # Create the result TransientVariable (if input inData is an AbstractVariable)
            # or masked array
            hasTime = None
            if inputIsVariable==1:
                hasTime = inData.getTime()
                if isinstance(self.dstGrid, list):
                    if len(self.dstGrid) == 2:
                        index = 0
                    else:
                        index = 1
                    from cdms2.coord import TransientAxis2D as T2D
                    lats = T2D(self.regridObj.dst_coords[index], id = 'lat')
                    lons = T2D(self.regridObj.dst_coords[index+1], id = 'lon')
                    grid = cdms2.hgrid.TransientCurveGrid(lats, lons, id = 'CurveGrid')

                shape = grid.shape
                axisList = list(grid.getAxisList())
                if inData.rank() == 2:
                    pass
                elif inData.rank() == 3:
                    if inData.getTime() is not None:
                        axisList = [inData.getTime()] + list(axisList)
                        shape = tuple(list(axisList[0].shape) + list(shape))
                    else:
                        axisList = [inData.getLevel()] + list(axisList)
                        shape = tuple(list(axisList[0].shape) + list(shape))
                elif inData.rank() == 4 and inData.getTime() is not None:
                        aL = [inData.getTime(), inData.getLevel()]
                        axisList = aL + list(axisList)
                        s = []
                        for a in aL:
                            s.append(a.shape)
                        shape = tuple(s + list(shape))
                else:
                    raise RegridError, \
                          'Ranks > 4 currently not supported though this API'
                axisList = tuple(axisList)

            # Loop over the time variable
            nTime = 1
            if hasTime is not None:
                nTime = len(inData.getTime())
                outVar = numpy.zeros(shape, inData.dtype)
                for iTime in range(nTime):
                    outVar[iTime, ...] = self.regridObj(inData[iTime, ...])
            else:
                outVar = self.regridObj(inData)

            # Correct the shape of output weights
            amskout = numpy.ma.ones(outVar.shape, numpy.bool8)
            if hasMask(inData):
                if inputIsVariable == 1:
                    if inData.fill_value is not None:
                        amskout = 1 - (outVar == inData.fill_value)

            # Set the missing data mask of the output array, if any.
            hasMissing = not numpy.ma.alltrue(numpy.ma.ravel(amskout))
            slabMask = None
            if hasMissing:
                slabMask = numpy.ma.where(numpy.ma.equal(amskout, 0), 1, 0)

            # Combine missing data mask and output grid mask
            # Note: slabMask and outMask are Boolean here
            if self.outMask is not None:
                outMask = numpy.logical_not(numpy.resize(self.outMask, outshape))
                if hasMissing:
                    outMask = numpy.ma.logical_or(outMask, slabMask)
            else:
                outMask = slabMask

            if inputIsVariable:
                result = cdms2.createVariable(outVar, mask = outMask,
                                              fill_value = inData.fill_value,
                                              axes = axisList,
                                              grid = grid,
                                              attributes = attrs, id = varid)
            else:
                result = numpy.ma.masked_array(outVar, mask = outMask, fill_value = missing)

        return result

    def __del__(self):
        if self.regridTool == 'esmp':
            pass

            # Why does this fail?
            # esmf.finalize()

