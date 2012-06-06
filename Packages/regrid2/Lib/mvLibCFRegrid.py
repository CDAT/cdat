"""
LibCF regridding class

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""

from cdms2 import gsRegrid
from regrid2 import GenericRegrid

class LibCFRegrid(GenericRegrid):
    """
    """
    def __init__(self, srcGrid, dstGrid, srcMask = None, 
                 srcBounds = None, **args):
        """
        Constructor
        @param srcGrid array
        @param dstGrid array
        """
        self.regridObj = gsRegrid.Regrid(srcGrid, dstGrid, 
                                         src_bounds = srcBounds,
                                         **args)
        if srcMask is not None: 
            self.regridObj.setMask(srcMask)

    def computeWeights(self, **args):
        """
        Compute interpolation weights
        """
        self.regridObj.computeWeights(**args)

    def setMask(self, mask):
        """
        Set the mask using 0 as unmasked 1 as masked (numpy masked array definition)
        @param mask array
        """
        gsRegrid

    def apply(self, srcData, dstData, srcDataMask = None, **args):
        """
        Regrid source to destination
        @param srcData array
        @param dstData array
        @param srcDataMask array
        """
        self.regridObj.apply(srcData, dstData)


