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

def _getCoordList(grid):
    """
    Return a coordinate list from a CDMS grid
    @return [lats, lons] 
    """
    lats = grid.getLatitude()
    lons = grid.getLongitude()
    shp = grid.shape
  
    # Initial try at dealing with Curvilinear grids.
    # Assume lat lon Order!
    if len(lats.shape) > 1:
        return lats, lons

    if grid.getAxis(0).isLatitude():
        # looks like order is lats, lons
        # turn into curvilinear, if need be
        if len(lats.shape) == 1:
            lats = regrid2.gsRegrid.getTensorProduct(lats[:], 0, shp)
        if len(lons.shape) == 1:
            lons = regrid2.gsRegrid.getTensorProduct(lons[:], 1, shp)
        return lats, lons

    # looks like order is lons, lats
    if len(lats.shape) == 1:
        lats = regrid2.gsRegrid.getTensorProduct(lats[:], 1, shp)
    if len(lons.shape) == 1:
        lons = regrid2.gsRegrid.getTensorProduct(lons[:], 0, shp)
    return lons, lats

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
        dgAxisList = dstGrid.getAxisList()[-2:]
    except:
        print "\n\nWARNING!!!\n" + \
              "Using [dstGrid.getLatitude(), dstGrid.getLongitude()]\n\n"
        dgAxisList = [dstGrid.getLatitude(), dstGrid.getLongitude()]

    return svAxisList + dgAxisList

    dstAxisList = []
    horizAxes = {}
    index = 0

    for a in srcVar.getAxisList():
        if a.isLatitude():
            horizAxes['lat_index'] = index
            dstAxisList.append(None)
        elif a.isLongitude(): 
            horizAxes['lon_index'] = index
            dstAxisList.append(None)
        else:
            dstAxisList.append(a)
        index += 1

    # Not a solution..., but it works for salinity
    # If curvilinear coordinates, lat-lon axes do not exist.
    # I am assuming lat-lon order here.
    if 'lat_index' not in horizAxes:
        dstAxisList.insert(0, None)
        horizAxes['lat_index'] = 1
    if 'lon_index' not in horizAxes:
        dstAxisList.insert(0, None)
        horizAxes['lon_index'] = 0
        
    # fill in the lat axis
    latShape = dstGrid.getLatitude().shape
    if len(latShape) > 1:
        # curvilinear, create some fake axes
        n = latShape[0]
        if horizAxes['lon_index'] < horizAxes['lat_index']:
            n = latShape[1]
        latAxis = createAxis(numpy.array([i for i in range(n)], numpy.float64),
                             id = 'lat_index')
        dstAxisList[horizAxes['lat_index']] = latAxis
    else:
        dstAxisList[horizAxes['lat_index']] = dstGrid.getLatitude()
    # fill in the lon axis
    lonShape = dstGrid.getLongitude().shape
    if len(lonShape) > 1:
        # curvilinear, create some fake axes
        n = lonShape[0]
        if horizAxes['lon_index'] < horizAxes['lon_index']:
            n = lonShape[1]
        lonAxis = createAxis(numpy.array([i for i in range(n)], numpy.float64),
                             id = 'lon_index')
        dstAxisList[horizAxes['lon_index']] = lonAxis
    else:
        dstAxisList[horizAxes['lon_index']] = dstGrid.getLongitude()


class CdmsRegrid:
    """
    Regridding switchboard, handles CDMS variables before handing off to 
    regridder. If a multidimensional variable is passed in, the apply step
    loops over the axes above the Lat (Y) -- Lon (X) coordinates
    """
    def __init__(self, srcGrid, dstGrid, 
                 regridMethod = 'Linear', regridTool = 'LibCF', 
                 srcBounds = None, srcGridMask = None, srcGridAreas = None,
                 dstBounds = None, dstGridMask = None, dstGridAreas = None,
                 **args):
        """
        Establish which regridding method to use, handles CDMS Variables before
        handing off to regridder. See specific tool for more information.

        @param srcGrid CDMS source grid
        @param dstGrid CDMS destination grid
        @param regridMethod Linear (all tools - Bi, tri), 
                            Conservative (ESMF Only)
                            Patch (ESMF Only)
        @param regridTool LibCF, ESMF, ...
        @param srcBounds source grid cell bounds
        @param srcGridMask array source mask, interpolation 
                           coefficients will not be computed for masked
                           points/cells.
        @param srcGridAreas array destination cell areas, only needed for 
                            conservative regridding
        @param dstBounds destination grid cell bounds
        @param dstGridMask array destination mask, interpolation 
                           coefficients will not be computed for masked
                           points/cells.
        @param dstGridAreas array destination cell areas, only needed for 
                            conservative regridding
        @param **args additional, tool dependent arguments
        """
        
        self.srcGrid = srcGrid
        self.dstGrid = dstGrid

        srcCoords = _getCoordList(srcGrid)
        dstCoords = _getCoordList(dstGrid)

        self.regridObj = regrid2.GenericRegrid(srcCoords, dstCoords, 
                                               regridMethod = regridMethod, 
                                               regridTool = regridTool,
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
        dstShape = list(srcVar.shape[:-2]) + list(self.dstGrid.shape)

        # interpolate the data
        dstData = numpy.zeros(dstShape, dtype = srcVar.dtype)
        self.regridObj.apply(srcVar.data, dstData, 
                             missingValue = missingValue, **args)

        # construct the axis list for dstVar
        dstAxisList = _getAxisList(srcVar, self.dstGrid)

        # harvest all the string attributes from srcVar
        attrs = {}
        for a in srcVar.attributes:
            v = srcVar.attributes[a]
            if type(v) is types.StringType:
                attrs[a] = v


        # create the transient variable
        dstVar = cdms2.createVariable(dstData, 
                                      mask = dstMask,
                                      fill_value = missingValue,
                                      axes = dstAxisList,
                                      grid = self.dstGrid,
                                      attributes = attrs, 
                                      id = srcVar.id + '_CdmsRegrid')
        
        
        return dstVar

