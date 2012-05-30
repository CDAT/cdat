#!/usr/bin/env python

"""
Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""

import numpy
import copy
import cdms2
from cdms2 import gsRegrid
import regrid2
from regrid2 import RegridError
import re

# Test for the presence of esmp/esmf.
ESMP_IMPORTED = False
try:
    from regrid2 import esmf
    try:
        import ESMP
        ESMP_IMPORTED = True
    except:
        pass
except:
    pass

MPI_IMPORTED = False
try:
    from pi4py import MPI
    MPI_IMPORTED = True
except:
    pass

def _makeGridList(grid):
    """
    Convert a cdms2 grid to a list of coordinates
    Using this because TransientRectGrids have no AxisList though the method
    exists
    @param grid The grid to be converted
    """
    retGrid = []
    nSpatial = 0
    if cdms2.isGrid(grid):
        # It is a grid. In cdms2 world, a grid is lon/lat (no elevation).
        nSpatial = 2
        retGrid = [grid.getLatitude(), grid.getLongitude()]
    elif isinstance(grid, list):
        # It is a list already
        nSpatial = len(grid)
        retGrid = copy.copy(grid)
    else:
        msg = 'regridder::_makeGridList: '
        msg += 'grid must be a list of coordinates or a cdms2 grid'
        raise RegridError, msg

    # Check the number of coordinates
    if nSpatial < 2:
        msg = 'regridder::_makeGridList: '
        msg += 'only one coordinate found. Regridding needs at least two.'
        raise RegridError, msg

    return retGrid, nSpatial

def _hasMask(data):
    """
    Does the data provided contain a mask?
    @param data
    """
    if hasattr(data, 'mask') and data.mask.size > 1:
        return True
    return False

def _makeCoordsFromBounds(coords):
    """
    Build nodal mesh.
    @param coords list of coordinates [..., y, x], x, y, ... can be axes or 
                  curvilinear coordinates.
    @return [.., yNodes, xNodes], (..., njNodes, niNodes) 
    """

    if not isinstance(coords, list):
        msg = 'regridder::_makeCoordsFromBounds: '
        msg += 'coordinates must be a list'
        raise RegridError, msg

    coordNodes = []
    dimNodes = []

    ndims = len(coords)
    # last coordinates can be curvilinear
    shp = coords[-1].shape
    dimNodes = [i + 1 for i in shp]
    rank = len(shp)

    bounds = []
    for c in coords:
        if hasattr(c, 'getBounds'):
            bounds.append(c.getBounds())
        elif hasattr(c, 'genGenericBounds'):
            bounds.append(c.genGenericBounds())
        else:
            msg = 'regridder::_makeCoordsFromBounds: '
            msg += 'bounds cannot be found or created'
            raise RegridError, msg


    if rank == 1:
        
        # list of axes
        dimNodes = [len(coords[i]) + 1 for i in range(ndims)]
        coordNodes = [numpy.zeros(dimNodes[i], numpy.float64) \
                          for i in range(ndims)]
        for i in range(ndims):
            coordNodes[i][:-1] = bounds[i][ :, 0]
            coordNodes[i][ -1] = bounds[i][-1, 1]
             
    elif rank == 2 and ndims == 2:

        # CF conventions assume counterclockwise ordering of bounds 
        # starting from the lower left corner
        coordNodes = [numpy.zeros(dimNodes, numpy.float64) \
                          for i in range(ndims)]
        for i in range(ndims):
            coordNodes[i][:-1, :-1] = bounds[i][ :,  :, 0]
            coordNodes[i][:-1,  -1] = bounds[i][ :, -1, 1] # right
            coordNodes[i][ -1,  -1] = bounds[i][-1, -1, 2] # top right corner
            coordNodes[i][ -1, :-1] = bounds[i][-1,  :, 3] # top

    elif rank == 3 and ndims == 3:

        # 3d curvilinear 
        coordNodes = [numpy.zeros(dimNodes, numpy.float64) \
                          for i in range(ndims)]
        for i in range(ndims):
            coordNodes[i][:-1, :-1, :-1] = bounds[i][ :,  :,  :, 0]
            coordNodes[i][:-1, :-1,  -1] = bounds[i][ :,  :, -1, 1]
            coordNodes[i][:-1,  -1,  -1] = bounds[i][ :, -1, -1, 2]
            coordNodes[i][:-1,  -1, :-1] = bounds[i][ :, -1,  :, 3]
            coordNodes[i][ -1, :-1, :-1] = bounds[i][-1,  :,  :, 4]
            coordNodes[i][ -1, :-1,  -1] = bounds[i][-1,  :, -1, 5]
            coordNodes[i][ -1,  -1,  -1] = bounds[i][-1, -1, -1, 6]
            coordNodes[i][ -1,  -1, :-1] = bounds[i][-1, -1,  :, 7]

    elif rank == 2 and ndims == 3:
        
        # 2d curvilinear and one vertical axis
        dimNodes[0] = len(coords[0]) + 1
        coordNodes = [numpy.zeros([dimNodes[0],], numpy.float64)] \
            + [numpy.zeros(dimNodes[1:], numpy.float64) \
                   for i in range(1, ndims)]

        coordNodes[0][:-1] = bounds[0][ :, 0]
        coordNodes[0][ -1] = bounds[0][-1, 1]
        for i in range(1, ndims):
            coordNodes[i][:-1, :-1] = bounds[i][ :,  :, 0]
            coordNodes[i][:-1,  -1] = bounds[i][ :, -1, 1] # right
            coordNodes[i][ -1,  -1] = bounds[i][-1, -1, 2] # top right corner
            coordNodes[i][ -1, :-1] = bounds[i][-1,  :, 3] # top
        
    else:
        msg = 'regridder::_makeCoordsFromBounds: '
        msg += 'some funky combination of axes and curvilinear coordinates'
        raise RegridError, msg

    return coordNodes, dimNodes

def _makeBoundsCurveList(grid):
    """
    Construct a curvilinear grid from the bounds of the given grid
    @param grid the grid. Needs to have getBounds()
    """
    if isinstance(grid, list):
        bn = []
        for g in grid:
            if not hasattr(g, 'getBounds'):
                b = cdms2.axis.createAxis(g)
                bn.append(b)
            else:
                bn.append(g)
        gridUse = bn
        # Create new grid and replace
        bounds, newDims = _makeCoordsFromBounds(gridUse)
    elif cdms2.isGrid(grid):
        g = [grid.getLatitude(), grid.getLongitude()]
        bounds, newDims = _makeCoordsFromBounds(g)
    else:
        msg = 'regridder::_makeBoundsCurveList: '
        msg += 'input grid must be a list of grids'
        raise RegridError, msg

    boundsCurve, dims = cdms2.gsRegrid.makeCurvilinear(bounds)
    return boundsCurve

class Regridder:
    """
    Regridding object. Wrapper class to run all of the different regridding
    tools available within UV-CDAT

    Optional keyword arguments for each type of regridder
            gsRegrid accepts nitermax and tolpos for computing the weights
            ESMP accepts src(dst)MaskValue and periodicity

    List of regridTools. cdms2.gsRegrid is the default
    "Libcf":    LibCF regridder. Handles curvilinear grids and 3D
    "gsRegrid": Same as libcf
            Optional args:
               src_bounds=None, mkCyclic=False, handleCut=False, diagnostics=False 

    "regrid2":  Original horizontal regridder
            Optional args:
                missing=None, order=None, mask=None, returnTuple=0

    "ESMF":     Earth System Modelling Framework.
                For more information
                http://www.earthsystemmodeling.org/users/python_doc/html/index.html,
    "ESMP":     Same as ESMF,
            Optional args:
                

    "SCRIP":    Not implemented in regridder. Run as stand alone for now
                SCRIP regridder.
            Optional Packages. ScripRegridder, ConservativeRegridder, BilinearRegridder
                               BicubicRegridder, DistwgtRegridder
    """
    def __init__(self, inGrid, outGrid, srcMask = None, dstMask = None,
                 regridTool = "gsRegrid", regridMethod = "bilinear", 
                 toCurvilinear = False, **args):
        """
        Constructor

        @param inGrid cdms2, ndarray variable
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
        @param toCurvilinear Return a curvilinear grid instead of the 
                        input grid
        @param **args Optional keyword arguments for each type of regridder
                        gsRegrid accepts nitermax and tolpos for computing the
                        weights
                        ESMP accepts src(dst)MaskValue and periodicity

        List of regridTools gsRegrid is the default
        "Libcf":    LibCF regridder. Handles curvilinear grids and 3D
        "gsRegrid": Same as libcf
                Optional args:
                   src_bounds=None, mkCyclic=False, handleCut=False, diagnostics=False 

        "regrid2":  Original horizontal regridder
                Optional args:
                    missing=None, order=None, mask=None, returnTuple=0

        "ESMF":     Earth System Modelling Framework.
                    For more information
                    http://www.earthsystemmodeling.org/users/python_doc/html/index.html,
        "ESMP":     Same as ESMF,
                Optional args:
                    

        "SCRIP":    Not implemented in regridder. Run as stand alone for now
                    SCRIP regridder.
                Optional Packages. ScripRegridder, ConservativeRegridder, BilinearRegridder
                                   BicubicRegridder, DistwgtRegridder
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
        self.srcMask = srcMask
        self.dstMask = dstMask
        self.srcMaskValue = None
        self.dstMaskValue = None
        self.outGrid = copy.copy(outGrid)
        self.toCurvilinear = toCurvilinear
        self.srcFractionPtr = None
        self.dstFractionPtr = None
        self.srcAreaPtr = None
        self.dstAreaPtr = None
        self.srcField = None
        self.dstField = None
        self.regrid = None  # The ESMP regrid object
        self.lats = []
        self.lons = []

        if re.match('regrid', rgTool, re.I):
            self.regridObj = regrid2.Horizontal(inGrid, outGrid)
            self.regridTool = 'regrid2'
        elif rgTool == 'scrip':
            msg = 'regridder::Regridder.__init__: '
            msg += 'SCRIP has not yet been implemented in Regrid'
            raise RegridError, msg

        elif re.match('esm', rgTool):
            if not ESMP_IMPORTED:
                msg = 'regridder::Regridder.__init__: '
                msg += 'ESMP is not installed or is unable to be imported'
                raise RegridError, msg
            self.regridTool = 'esmp'

            # Set some required values
            self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
            self.staggerloc = ESMP.ESMP_STAGGERLOC_CENTER
            self.coordSys = ESMP.ESMP_COORDSYS_SPH_DEG
            self.periodicity = None
            makeCyclic = False
            handleCut = False
            srcBounds = None
            dstBounds = None
            srcMaskValues = None
            dstMaskValues = None

            for arg in args:
                if re.search('unmappedaction', arg.lower()):
                    unMappedAction = args[arg]
                    if re.search('error', unMappedAction):
                        self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_ERROR
                if re.search('periodicity', arg.lower()):
                    self.periodicity = args[arg]
                if re.search('cycl', arg.lower()):
                    makeCyclic = True
                if re.search('handle', arg.lower()):
                    handleCut = True
                if re.search('staggerloc', arg.lower()):
                    if re.search('corner', args[arg].lower()):
                        self.staggerloc = ESMP.ESMP_STAGGERLOC_CORNER
                    elif re.search('center', args[arg].lower()):
                        self.staggerloc = ESMP.ESMP_STAGGERLOC_CENTER
                    else:
                        msg = 'regridder::Regridder.__init__: '
                        msg += 'invalid stagger location ("center" or "corner")'
                        raise RegridError, msg
                if re.search('coordsys', arg.lower()):
                    if isinstance(args[arg], str):
                        if re.search('cart', args[arg].lower()):
                            self.coordSys = ESMP.ESMP_COORDSYS_CART
                        elif re.search('deg', args[arg].lower()):
                            self.coordSys = ESMP.ESMP_COORDSYS_SPH_DEG
                        elif re.search('rad', args[arg].lower()):
                            self.coordSys = ESMP.ESMP_COORDSYS_SPH_RAD
                        else:
                            msg = 'regridder::Regridder.__init__: '
                            msg += 'invalid coordinate system ("cart", "deg", or "rad")'
                            raise RegridError, msg

            # If xxxMaskValues in arguments, set them for later in __call__
            for arg in args.keys():
                if re.search('srcmaskvalue', arg.lower()):
                    self.srcMaskValue = numpy.array([args[arg]], 
                                                     dtype = numpy.int32)
                if re.search('dstmaskvalue', arg.lower()):
                    self.dstMaskValue = numpy.array([args[arg]], 
                                                    dtype = numpy.int32)
            if srcMask is not None and self.srcMaskValue is None:
                self.srcMaskValue = [1]
            if dstMask is not None and self.dstMaskValue is None:
                self.dstMaskValue = [1]

            # Set the regridding method - Bilinear / Conservative
            if regridMethod is None or regridMethod.lower() == 'bilinear':
                self.regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR
            else:
                self.regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE

            # Want 2D coordinates in a list [lat, lon]
            # Choices list, TransientAxis2D, TransientRectAxis
            srcGrid, self.srcRank = _makeGridList(inGrid)
            if self.srcRank > 1:
                srcGrid, srcSpatial = cdms2.gsRegrid.makeCurvilinear(srcGrid)

            dstGrid, self.dstRank = _makeGridList(outGrid)
            if self.dstRank > 1:
                dstGrid, dstSpatial = cdms2.gsRegrid.makeCurvilinear(dstGrid)

            # Convert bounds to curvilinear grids
            srcBoundsCurveList = None
            dstBoundsCurveList = None
            self.location = ESMP.ESMP_STAGGERLOC_CORNER
            # Dont need bounds for bilinear
            if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
                if srcBounds is None:
                    srcBoundsCurveList = _makeBoundsCurveList(inGrid)
                if dstBounds is None:
                    dstBoundsCurveList = _makeBoundsCurveList(self.outGrid)

            # Use non periodic boundaries unless the user says otherwise
            # not set by the user.
            if self.periodicity is None:
                self.periodicity = 0

            # Create the ESMP grids
            self.srcGrid = esmf.EsmfStructGrid(srcGrid[0].shape,
                                        coordSys = self.coordSys,
                                        periodicity = self.periodicity)
            self.srcGrid.setCoords(srcGrid, staggerloc = self.staggerloc)
            self.srcGrid.shape = srcGrid[0].shape
            if srcMask is not None: self.srcGrid.setCellMask(srcMask)

            self.dstGrid = esmf.EsmfStructGrid(dstGrid[0].shape,
                                           periodicity = self.periodicity,
                                           coordSys = self.coordSys)
            self.dstGrid.setCoords(dstGrid, staggerloc = self.staggerloc) 
            self.dstGrid.shape = dstGrid[0].shape
            if dstMask is not None: self.dstGrid.setCellMask(dstMask)

            # Populate the grid corners. Conservative only.
            if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
                self.srcGrid.setCoords(srcBoundsCurveList,
                                    staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
                self.dstGrid.setCoords(dstBoundsCurveList,
                                    staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)

        elif rgTool == 'gsregrid' or rgTool == 'libcf':
            # Prep in and out grid for gsRegrid!
            self.srcGrid, self.srcRank = _makeGridList(inGrid)
            self.dstGrid, self.dstRank = _makeGridList(outGrid)

            if self.dstRank != self.srcRank:
                msg = 'regridder::Regridder.__init__: '
                msg += 'outGrid rank (%d) != inGrid rank (%d)' % \
                    (self.dstRank, self.srcRank)
                raise RegridError, msg

            # Create the regrid object
            ro = cdms2.gsRegrid.Regrid(self.srcGrid, self.dstGrid)

            # Set the source mask
            if srcMask is not None:

                if len(srcMask.shape) > 3:
                    msg = 'regridder:Regridder.__init__: '
                    msg += 'ranks greater than three are not supported'
                    raise RegridError, msg

                self.srcMask = numpy.array(srcMask, dtype = numpy.int32)
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
            msg = 'regridder::Regridder.__init__: '
            msg += 'invalid regrid tool ("regrid2", "gsRegrid", or "esmf")'
            raise RegridError, msg

    def applyEsmf(self, inData, **args):
        """
        Interpolate using ESMF
        @param inData The input data
        @param args optional arguments
        @return outVar, outMask, missing
        """
        outVar = None
        outMask = None
        missing = None

        if not ESMP_IMPORTED:
            msg = 'regridder::Regridder.applyEsmf: '
            msg += 'ESMP is not installed or is unable to be imported'
            raise RegridError, msg

        if self.location is None:
            location = ESMP.ESMP_STAGGERLOC_CENTER
        else:
            location = self.location

        location = ESMP.ESMP_STAGGERLOC_CENTER

        # Make sure we are passing a ndarray
        try:
            iid = inData.id
            diid = inData.id
        except:
            iid = "Source_Data"
            diid = "Desintation_Data"

        # Create the ESMF Fields

        self.srcField = esmf.EsmfStructField(self.srcGrid, iid,
                                             numpy.array(inData),
                                             staggerloc = location)
        # Convert mask y, x
        outShape = self.dstGrid.shape
        outVar = numpy.zeros(outShape, inData.dtype)
        self.dstField = esmf.EsmfStructField(self.dstGrid, diid,
                                           outVar,
                                           staggerloc = location)

        srcFrac = esmf.EsmfStructField(self.srcGrid, diid,
                             staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        dstFrac = esmf.EsmfStructField(self.dstGrid, diid,
                             staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            srcArea = esmf.EsmfStructField(self.srcGrid, diid,
                             staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
            dstArea = esmf.EsmfStructField(self.dstGrid, diid,
                             staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
        if self.regridMethod is not None:
            method = self.regridMethod
        else:
            method = ESMP.ESMP_REGRIDMETHOD_BILINEAR

        if self.unMappedAction is not None:
            unMappedAction = self.unMappedAction
        else:
            unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE

        # Regrid
        self.regrid = esmf.EsmfRegrid(self.srcField, self.dstField,
                            srcFrac = srcFrac,
                            dstFrac = dstFrac,
                            srcMaskValues = self.srcMaskValue,
                            dstMaskValues = self.dstMaskValue,
                            regridMethod = method,
                            unMappedAction = unMappedAction)

        # Call the regrid procedure
        self.regrid()

        # Get the regridded variable
        outVar.flat = self.dstField.getPointer()

        # Set the output mask if available
        outMask = None
        if _hasMask(inData) or self.outMask is not None:
            attrs = ('missing_value','fill_value',)
            missing = None
            for att in attrs:
                if hasattr(inData, att):
                    missing = getattr(inData, att)
            if missing is not None: outMask = (outVar == missing)
        elif self.dstMask is None:
            outMask = self.dstMask

        # Get the coordinates
        self.srcFractionPtr = numpy.ones(inData.shape, dtype = inData.dtype)
        self.dstFractionPtr = numpy.ones(outShape, dtype = inData.dtype)
        self.srcFractionPtr.flat = srcFrac.getPointer()
        self.dstFractionPtr.flat = dstFrac.getPointer()
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            self.srcAreaPtr = numpy.ones(inData.shape, dtype = inData.dtype)
            self.dstAreaPtr = numpy.ones(outShape, dtype = inData.dtype)

            ESMP.ESMP_FieldRegridGetArea(srcArea.field)
            ESMP.ESMP_FieldRegridGetArea(dstArea.field)

            self.srcAreaPtr.flat = srcArea.getPointer()
            self.dstAreaPtr.flat = dstArea.getPointer()

        self.lats = numpy.reshape(self.dstGrid.getCoords(0, 
                                                         ESMP.ESMP_STAGGERLOC_CENTER), 
                                  outVar.shape)
        self.lons = numpy.reshape(self.dstGrid.getCoords(1, 
                                                         ESMP.ESMP_STAGGERLOC_CENTER), 
                                  outVar.shape)
        
        return outVar, outMask, missing

    def applyLibcf(self, inData, **args):
        """
        Interpolate using LIBCF
        @param inData The input data
        """
        pass

    def __call__(self, inData, **args):
        """
        Apply the interpolation.
        @param inData The input data
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
            if isinstance(inData, TransientVariable) and \
               inData.iscontiguous() is False:
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

        useResult = False
        if self.regridTool == 'regrid2':
            result = self.regridObj(inData, args)
            useResult = True
        elif self.regridTool == 'scrip':
            pass
        elif self.regridTool == 'esmp':
            outVar, outMask, missing = self.applyEsmf(inData, **args)

        elif self.regridTool == 'gsregrid':

            # The data has a mask and the mask has not been set previously
            # If the mask is then set, the weights must be computed...
            if _hasMask(inData) and self.regridObj.maskSet is False:
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

            # Create the result TransientVariable 
            # (if input inData is an AbstractVariable)
            # or masked array
            hasTime = None
            hasLevel = None
            if inputIsVariable==1:
                hasTime = inData.getTime()   # None if empty
                hasLevel = inData.getLevel() # None if empty
                if isinstance(self.dstGrid, list):
                    if len(self.dstGrid) == 2:
                        index = 0
                    else:
                        index = 1
                    from cdms2.coord import TransientAxis2D as T2D
                    self.lats = T2D(self.regridObj.dst_coords[index], id = 'lat')
                    self.lons = T2D(self.regridObj.dst_coords[index+1], id = 'lon')
                    grid = cdms2.hgrid.TransientCurveGrid(self.lats, self.lons, 
                                                          id = 'CurveGrid')

                outShape = grid.shape
                axisList = list(grid.getAxisList())
                if inData.rank() == 2:
                    pass
                elif inData.rank() == 3:
                    if inData.getTime() is not None:
                        axisList = [inData.getTime()] + list(axisList)
                        outShape = tuple(list(axisList[0].shape) + \
                                         list(outShape))
                    else:
                        axisList = [inData.getLevel()] + list(axisList)
                        outShape = tuple(list(axisList[0].shape) + \
                                         list(outShape))
                elif inData.rank() == 4 and inData.getTime() is not None:
                    axisList = [inData.getTime(), inData.getLevel()] + \
                                list(axisList)
                    outShape = inData.shape
                else:
                    msg = 'regridder::Regridder.__call__: '
                    msg += 'ranks > 4 currently not supported'
                    raise RegridError, msg

                axisList = tuple(axisList)
            else:
                outShape = self.regridObj.dst_dims[:]

            # Create the output data array. Assuming time in first index
            if len(outShape) != len(inData.shape):
                dd = [d for d in self.regridObj.dst_dims[:]]
                outShape = [inData.shape[0]] + dd
            outVar = numpy.ones(outShape, dtype = inData.dtype)
            if hasattr(inData, 'missing_value'):
                outVar = outVar * inData.missing_value
            elif hasattr(inData, 'fill_value'):
                outVar = outVar * inData.fill_value
            if len(outVar.shape) != len(inData.shape):
                msg = 'regridder::Regridder.__call__: '
                msg += 'outVar and inData have different shapes outVar.shape'
                msg += '%s = %s inVar.shape = %s: ' % \
                             (string, str(outVar.shape), str(inData.shape))
                raise RegridError, msg

            # Convert to masked array
            outVar = numpy.ma.array(outVar, mask = self.outMask)

            # Loop over the time or level variable
            # regridder will regrid in the horizontal axes, while using the 
            # existing time and level information
            rank = len(inData.shape)
            nTime = 1
            if rank == 2:
                # lat-lon grid
                self.regridObj(inData, outVar)
            elif rank == 3:
                if hasTime is not None:
                    #Time without Level
                    nTime = len(inData.getTime())
                    for iTime in range(nTime):
                        self.regridObj(inData[iTime, ...], outVar[iTime, ...])
                elif hasLevel is not None:
                    # Level without time
                    nLevel = len(inData.getLevel())
                    for iLevel in range(nLevel):
                        self.regridObj(inData[iLevel, ...], outVar[iLevel, ...])
                else:
                    msg = 'regridder::Regridder.__call__: '
                    msg += '3rd most slowly varying dimension is neither time nor level'
                    raise msg

            elif rank == 4:
                if hasTime is None or hasLevel is None:
                    msg = 'regridder::Regridder.__call__: '
                    msg += 'rank 4 data must have time and level'
                    raise RegridError, msg

                else:
                    nLevel = len(inData.getLevel())
                    nTime = len(inData.getTime())
                    for iTime in range(nTime):
                        for iLevel in range(nLevel):
                            self.regridObj(inData[iTime, iLevel, ...], 
                                           outVar[iTime, iLevel, ...])
                    
            # Correct the shape of output weights
            amskout = numpy.ma.ones(outVar.shape, numpy.bool8)
            if _hasMask(inData):
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
                outMask = numpy.logical_not(numpy.resize(self.outMask, 
                                                         outShape))
                if hasMissing:
                    outMask = numpy.ma.logical_or(outMask, slabMask)
            else:
                outMask = slabMask

        # Return the output variable from ESMP or gsRegrid, or the result from
        # regrid2
        
        if isinstance(self.outGrid, list):
            if re.search('transientaxis2d', str(type(self.outGrid[0])).lower()):
                self.toCurvilinear = True
            
        if inputIsVariable and not self.toCurvilinear:
            # Use the output grid
            attrs = inData.attributes
            grid = None
            if isinstance(self.outGrid, list):
                axes = self.outGrid
                if len(inData.shape) > 2:
                    for a in inData.getOrder():
                        if a == 'z':
                            axes.insert(0, inData.getLevel())
                        if a == 't':
                            axes.insert(0, inData.getTime())
            elif cdms2.isGrid(self.outGrid):
                grid = self.outGrid
                try:
                    # Method exists but is not always implemented 
                    # e.g. TransientRectGrid
                    axes = self.outGrid.getAxisList()
                except:
                    axes = []
                    for a in inData.getOrder():
                        if a == 'x':
                            axes.append(self.outGrid.getLongitude())
                        if a == 'y':
                            axes.append(self.outGrid.getLatitude())
                        if a == 'z':
                            axes.append(inData.getLevel())
                        if a == 't':
                            axes.append(inData.getTime())
                    
                    if len(axes) < 2:
                        axes = []
                        for a in grid.getOrder():
                            if a == 'x':
                                axes.append(self.outGrid.getLongitude())
                            if a == 'y':
                                axes.append(self.outGrid.getLatitude())
                            if a == 'z':
                                axes.append(inData.getLevel())
                            if a == 't':
                                axes.append(inData.getTime())

            result = cdms2.createVariable(outVar, mask = outMask,
                                          fill_value = inData.fill_value,
                                          axes = axes,
                                          grid = grid,
                                          attributes = attrs, id = varid)

        elif self.toCurvilinear:
            # Make a curvilinear Grid. Overrides inputIsVariable
            lat = cdms2.coord.TransientAxis2D(self.lats, id = 'lat')
            lon = cdms2.coord.TransientAxis2D(self.lons, id = 'lon')
            grid = cdms2.hgrid.TransientCurveGrid(lat, lon, id = 'CurveGrid')
            result = cdms2.createVariable(outVar, mask = outMask,
                                          fill_value = inData.fill_value,
                                          axes = grid.getAxisList(),
                                          grid = grid,
                                          attributes = attrs, id = varid)
        elif useResult:
            pass # Place holder
        else:
            result = numpy.ma.masked_array(outVar, mask = outMask, 
                                           fill_value = missing)

        return result
