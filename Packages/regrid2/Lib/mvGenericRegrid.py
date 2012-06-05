"""
Generic Regrid class.

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""
import regrid2
import re

class GenericRegrid:
    """
    Generic Regrid class.
    """
    def __init__(self, srcGrid, dstGrid, regridMethod = 'Linear', 
                 regridTool = 'LibCF',
                 srcMask = None, srcBounds = None, srcAreas = None,
                 dstMask = None, dstBounds = None, dstAreas = None,
                 **args):
        """
        Constructor. Grids, [bounds, masks and areas if passed] must be in the 
        correct shape for the selected regridder
        @param srcGrid array
        @param dstGrid array
        @param regridMethod Linear (bi, tri,...) default or Conservative
        @param regridTool LibCF (gsRegrid), ESMP (ESMF)
        @param srcMask array of same shape as srcGrid
        @param srcBounds array of same shape as srcGrid
        @param srcAreas array of same shape as srcGrid
        @param dstMask array of same shape as dstGrid
        @param dstBounds array of same shape as dstGrid
        @param dstAreas array of same shape as dstGrid
        """

        if re.search('libcf', regridTool.lower()) or \
           re.search('gsreg', regridTool.lower()):
            self.tool = regrid2.LibCFRegrid(srcGrid, dstGrid, 
                 srcMask = srcMask, srcBounds = srcBounds)
        elif re.search('esm', regridTool.lower()):
            self.tool = regrid2.ESMFRegrid(srcGrid, dstGrid, 
                 srcMask = srcMask, srcBounds = srcBounds, srcAreas = srcAreas,
                 dstMask = dstMask, dstBounds = dstBounds, dstAreas = dstAreas,
                 **args)
    
    def setValidMask(self, mask):
        """
        Set a valid mask.
        @param mask 0 - Valid 1 - Invalid (numpy masked array definition)
        """
        self.tool.setValidMask(mask)

    def computeWeights(self):
        """
        Compute Weights
        """
        self.tool.computeWeights()

    def apply(self, srcData, dstData, **args):
        """
        Regrid source to destination
        @param srcData array
        @param dstData array
        """
        self.tool.apply(srcData, dstData, **args)



