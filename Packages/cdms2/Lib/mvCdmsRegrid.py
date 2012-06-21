"""
Macro regridding class

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""
import types
import re
import numpy
import cdms2
import regrid2

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

def _getBoundList(coordList):
    """
    Return a list of bounds built from a list of coordinates
    @param coordList coordinate list, should have getBounds()
    @return [latBounds, lonBounds]
    """
    bounds = []
    for c in coordList:
        bnds = _buildBounds(c.getBounds()[:])
        bounds.append(bnds)

    return bounds

def _getCoordList(grid):
    """
    Return a CDMS coordinate list from a CDMS grid
    @return lats, lons
    """
    lats = grid.getLatitude()
    lons = grid.getLongitude()
  
    if len(lats.shape) == 1 or len(lats.shape) == 1:
        # have axes, need to convert to curvilinear grid
        cgrid = grid.toCurveGrid()
        lats = cgrid.getLatitude()
        lons = cgrid.getLongitude()

    # we always want the coordinates in that order
    return lats, lons

def _getAxisList(srcVar, dstGrid):
    """
    Get the list of axes from a variable and a grid
    @param srcVar the variable from which all axes other than lat/lon 
                  will be taken from
    @param dstGrid target, horizontal grid
    @return variable with non-horizontal axes from srcVar and horizontal axes
            from dstGrid
    """

    # harvest the axis list form srcVar, start with all axes other than 
    # lat/lon
    
    # ASSUMING y, x axes are the last two axes.
    # From the source axis list get every axis up to these.
    svAxisList = srcVar.getAxisList()[:-2]

    # From the destination grid get the horizontal axes!
    try:
        dgAxisList = list(dstGrid.getAxisList()[-2:])
    except:
        dgAxisList = []
        dgAxisList.append(dstGrid.getLatitude())
        dgAxisList.append(dstGrid.getLongitude())

    return svAxisList + dgAxisList

class CdmsRegrid:
    """
    Regridding switchboard, handles CDMS variables before handing off to 
    regridder. If a multidimensional variable is passed in, the apply step
    loops over the axes above the Lat (Y) -- Lon (X) coordinates
    """
    def __init__(self, srcGrid, dstGrid, 
                 regridMethod = 'linear', regridTool = 'libCF', 
                 srcGridMask = None, srcGridAreas = None,
                 dstGridMask = None, dstGridAreas = None,
                 **args):
        """
        Establish which regridding method to use, handles CDMS Variables before
        handing off to regridder. See specific tool for more information.

        @param srcGrid CDMS source grid
        @param dstGrid CDMS destination grid
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
        self.regridMethod = regridMethod
        if re.search( 'conserv', regridMethod.lower()):
            srcBounds = _getBoundList(srcCoords)
            dstBounds = _getBoundList(dstCoords)

        # If the srcGridMask has multiple dimensions, pick the first level
        # of the source mask
        srcGridMask0 = srcGridMask
        if srcGridMask is not None:
            string = "srcGridMask[" + "0,"*(len(srcGridMask.shape)-2)+"...]"
            srcGridMask0 = eval(string)

        self.regridObj = regrid2.GenericRegrid(srcCoords, dstCoords, 
                                               regridMethod = regridMethod, 
                                               regridTool = regridTool,
                                               srcGridMask = srcGridMask0, 
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
        dstShape = list(srcVar.shape[:-2]) + list(self.dstGrid.shape)

        # establish the destination data. Initialize to missing values or 0.
        dstData = numpy.ones(dstShape, dtype = srcVar.dtype)
        if missingValue is not None: 
            dstData *= missingValue
        else: 
            dstData *= 0.0
        
        # interpolate the data
        self.regridObj.apply(srcVar.data, dstData, 
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
        
        if re.search(self.regridMethod.lower(), 'conserv'):
            self.srcGridAreas = self.regridObj.getSrcAreas(rootPe = 0)
            self.dstGridAreas = self.regridObj.getDstAreas(rootPe = 0)
            self.srcFractions = self.regridObj.getSrcAreaFractions(rootPe = 0)
            self.dstFractions = self.regridObj.getDstAreaFractions(rootPe = 0)

        return dstVar

