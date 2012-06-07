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
from regrid2 import GenericRegrid

class Regridder:
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
        self.regridMethod = regridMethod

        self.regridObj = regrid2.mvGenericRegrid(srcGrid, dstGrid, 
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
        dstShape = list(srcVar.shape[:-2]) + list(self.dstGrid.shape())

        srcMaskFArray = None
        dstMaskFArray = None
        dstMask = None
        if missing_value is not None:
            srcMaskFArray = numpy.ones(self.srcGrid.shape, dtype = srcVar.dtype)
            dstMaskFArray = numpy.ones(self.dstGrid.shape, dtype = srcVar.dtype)
            # interpolate the data mask
            self.regridObj.apply(scrMaskFArray, dstMaskFArray, **args)
            # set the destination data mask
            if re.search('linear', self.regridMethod, re.I):
                # nodal 
                dstMask = numpy.array(dstMaskFArray > 0.0, dtype = numpy.int32)
            else:
                # cell or conservative
                dstMask = numpy.array(dstMaskFArray == 1.0, dtype = numpy.int32)

        # interpolate the data
        dstData = numpy.zeros(dstShape, dtype = srcVar.dtype)
        self.regridObj.apply(srcVar.data, dstVar.data, **args)

        # set masked data values to missing_value
        if dstMask is not None and missingValue is not None:
             dstData *= (1 - dstMask)
             dstData += dstMask * missingValue

        # create axis list

        # harvest all the string attributes from srcVar
        attrs = {}
        for a in srcVar.attributes:
            v = srcVar.attributes[a]
            if type(v) is types.StringType:
                attrs[a] = v
                
        # harvest the axis list form srcVar, start with all axes other than 
        # lat/lon
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
        # fill in the lat axis
        latShape = self.dstGrid.getLatitude().shape
        if len(latShape) > 1:
            # curvilinear, create some fake axes
            n = latShape[0]
            if horizAxes['lon_index'] < horizAxes['lat_index']:
                n = latShape[1]
            latAxis = createAxis(numpy.array([i for i in range(n)], numpy.float64),
                                 id = 'lat_index')
            dstAxisList[horizAxes['lat_index']] = latAxis
        else:
            dstAxisList[horizAxes['lat_index']] = self.dstGrid.getLatitude()
        # fill in the lon axis
        lonShape = self.dstGrid.getLongitude().shape
        if len(lonShape) > 1:
            # curvilinear, create some fake axes
            n = lonShape[0]
            if horizAxes['lon_index'] < horizAxes['lon_index']:
                n = lonShape[1]
            lonAxis = createAxis(numpy.array([i for i in range(n)], numpy.float64),
                                 id = 'lon_index')
            dstAxisList[horizAxes['lon_index']] = lonAxis
        else:
            dstAxisList[horizAxes['lon_index']] = self.dstGrid.getLongitude()

        # create the transient variable
        dstVar = cdms2.createVariable(dstData, 
                                      mask = dstMask,
                                      fill_value = missing_value,
                                      axes = dstAxisList,
                                      grid = self.dstGrid,
                                      attributes = attrs, 
                                      id = srcVar.id + '_mvRegridder')
        
        
        return dstVar

