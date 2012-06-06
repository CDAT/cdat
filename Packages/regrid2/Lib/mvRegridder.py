"""
Macro regridding class

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""
import re
from regrid2 import GenericRegrid

def getCDMSVarInfo():
    pass

def arrayToCDMSVar():
    pass

class Regridder:
    """
    Regridding switchboard, handles CDMS Variables before handing off to 
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

        @param srcGrid list of coordinates [[Additional Axes], Lat, Lon]
                       node based for LibCF,
                       cell based coordinates for ESMF
        @param dstGrid list of coordinates [[Additional Axes], Lat, Lon]
                       node based for LibCF,
                       cell based coordinates for ESMF
        @param regridMethod Linear (all tools - Bi, tri), 
                            Conservative (ESMF Only)
                            Patch (ESMF Only)
        @param regridTool LibCF, ESMF, regrid2, SCRIP
		@param srcBounds Source bounds in same order as srcGrid
		@param srcGridMask array Source mask, tool dependent.
                      If the data to be regridded is a masked array, the mask
                      will be set during the __call__ method
		@param srcGridAreas array Source area, tool dependent
        @param dstBounds Destination bounds in same order as dstGrid 
		@param dstGridMask Destination mask, tool dependent
		@param dstGridAreas Destination area, tool dependent
        @param **args Tool dependent arguments
        """
        
        if re.search('regrid2', regridTool.lower()):
            self.regridMethod = 'regrid2'
            self.regridObj = regrid2.horizontal(srcGrid, dstGrid)
        elif re.search('gsr', regridTool.lower() or \
             re.search('libcf', regridTool.lower() or \
             re.search('esm', regridTool.lower()): 

            self.regridMethod = 'mvGeneric'
            # ensure grids are a list of curvilinear coordinates in 
            # lat-lon order
            self.regridObj = regrid2.mvGenericRegrid( srcGrid, dstGrid, 
                                  regridMethod = regridMethod, 
                                  regridTool = regridTool,
                                  srcGridMask = srcGridMask, srcBounds = srcBounds, 
                                  srcGridAreas = srcGridAreas,
                                  dstGridMask = dstGridMask, dstBounds = dstBounds, 
                                  dstGridAreas = dstGridAreas, **args )
            self.regridObj.computeWeights(**args)
        else:
            raise RegridError, 'Supported RegridTools are LibCF, ESMF, Regrid2'

    def __call__(self, srcData, **args):
        """
        Apply the regridding. Loop over additional axes here.
        @param srcData CDMS var or array
        @param dstData array. 
        @param **args Tool dependent arguments
        @return CDMS var if srcData is CDMS var, otherwise an array
        """
        
        if self.regridMethod == 'regrid2':
            result = self.regridObj(srcData, **args)
            return = result
        elif self.regridMethod == 'mvGeneric':
            # Handle the variable. Retrieve CDMS information if available
            
            # Create Destination array from dstGrid shape and 
            # srcData axes other than its grid

            # Loop over additional axes.
            self.regridObj.apply(srcData, dstData, **args)

            # Convert the dstData to a CMDS Variable if srcIsVar

            return dstData

