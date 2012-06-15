"""
Generic Regrid class.

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""
import operator
import numpy
import string

import regrid2
import re
from distarray import MultiArrayIter

class GenericRegrid:
    """
    Generic Regrid class.
    """
    def __init__(self, srcGrid, dstGrid, 
                 regridMethod, 
                 regridTool,
                 srcGridMask = None, srcBounds = None, srcGridAreas = None,
                 dstGridMask = None, dstBounds = None, dstGridAreas = None,
                 **args):
        """
        Constructor. 
        @param srcGrid array
        @param dstGrid array
        @param regridMethod linear (bi, tri,...) default or conservative
        @param regridTool 'libcf' or 'esmf'
        @param srcGridMask array of same shape as srcGrid
        @param srcBounds array of same shape as srcGrid
        @param srcGridAreas array of same shape as srcGrid
        @param dstGridMask array of same shape as dstGrid
        @param dstBounds array of same shape as dstGrid
        @param dstGridAreas array of same shape as dstGrid
        @param **args additional arguments to be passed to the 
                      specific tool
                      'libcf': mkCyclic={True, False}, handleCut={True,False}
                      'esmf': .... TO FILL IN
        """

        self.nGridDims = len(srcGrid)
        self.regridMethod = regridMethod

        if len(srcGrid) != len(dstGrid):
            msg = 'mvGenericRegrid.__init__: mismatch in number of dims'
            msg += ' len(srcGrid) = %d != len(dstGrid) = %d' % \
                (self.nGridDims, len(dstGrid))
            raise regrid2.RegridError, msg

        # parse the options
        if re.search('libcf', regridTool.lower()) or \
           re.search('gsreg', regridTool.lower()):
            # LibCF
            self.tool = regrid2.LibCFRegrid(srcGrid, dstGrid, 
                 srcGridMask = srcGridMask, srcBounds = srcBounds, **args)
        elif re.search('esm', regridTool.lower()):
            # ESMF
            staggerLoc = args.get('staggerLoc', None)
            periodicity = args.get('periodicity', 1)
            coordSys = args.get('coordSys', 'deg')
            self.tool = regrid2.ESMFRegrid(srcGrid, dstGrid,
                  regridMethod = regridMethod, 
                  staggerLoc = staggerLoc,
                  periodicity = periodicity,
                  coordSys = coordSys,                 
                  srcGridMask=srcGridMask, srcBounds=srcBounds, 
                  srcGridAreas=srcGridAreas,
                  dstGridMask=dstGridMask, dstBounds=dstBounds, 
                  dstGridAreas=dstGridAreas,
                  **args)
    
    def computeWeights(self, **args):
        """
        Compute Weights
        """
        self.tool.computeWeights(**args)

    def apply(self, srcData, dstData, missingValue = None, **args):
        """
        Regrid source to destination
        @param srcData array (input)
        @param dstData array (output)
        @param missingValue if not None, then data mask will be interpolated
                            and data value set to missingValue when masked
        """

        # assuming the axes are the slowly varying indices
        srcHorizShape = srcData.shape[-self.nGridDims :]
        dstHorizShape = dstData.shape[-self.nGridDims :]
    
        srcDataMaskFloat = None
        dstDataMaskFloat = None
        dstMask = None
        if missingValue is not None:
            srcDataMaskFloat = numpy.zeros(srcHorizShape, srcData.dtype)
            dstDataMaskFloat = numpy.zeros(dstHorizShape, dstData.dtype)
             
        nonHorizShape = srcData.shape[: -self.nGridDims]

        if len(nonHorizShape) == 0:

            #
            # no axis... just call apply 
            #

            self.tool.apply(srcData, dstData, **args)

            # adjust for masking
            if missingValue is not None:
                srcDataMaskFloat[:] = (srcData == missingValue)
                # interpolate mask
                self.tool.apply(srcDataMaskFloat, dstDataMaskFloat, **args)
                if re.search('conserv', self.regridMethod, re.I):
                    dstMask = numpy.array( (dstDataMaskFloat == 1), numpy.int32 )
                else:
                    dstMask = numpy.array( (dstDataMaskFloat > 0), numpy.int32 )
                dstData *= (1 - dstMask)
                dstData += dstMask*missingValue

        else:

            nonHorizShape2 = dstData.shape[: -self.nGridDims]
            if not numpy.all(nonHorizShape2 == nonHorizShape):
                msg = 'mvGenericRegrid.apply: axes detected '
                msg += 'but %s != %s ' % (str(nonHorizShape2),
                                          str(nonHorizShape))
                raise regrid2.RegridError, msg

            #
            # iterate over all axes
            #

            # create containers to hold input/output values
            # (a copy is essential here)
            zros = '[' + ('0,'*len(nonHorizShape)) + '...]'
            indata = numpy.array(eval('srcData' + zros))
            outdata = numpy.array(eval('dstData' + zros))

            # now iterate over all non lat/lon coordinates
            diags = []
            diag = 0
            for it in MultiArrayIter(nonHorizShape):

                indices = it.getIndices()
                slce = '[' 
                slce += reduce(operator.add, ['%d,'%i for i in indices])
                slce += '...]'
                indata = eval('srcData' + slce)

                # interpolate, using the appropriate tool
                self.tool.apply(indata, outdata, **args)

                # adjust for masking
                if missingValue is not None:
                    srcDataMaskFloat[:] = (indata == missingValue)
                    # interpolate mask
                    self.tool.apply(srcDataMaskFloat, dstDataMaskFloat, **args)
                    if re.search('conserv', self.regridMethod, re.I):
                        # cell interpolation
                        dstMask = numpy.array( (dstDataMaskFloat == 1), numpy.int32 )
                    else:
                        # nodal interpolation
                        dstMask = numpy.array( (dstDataMaskFloat > 0), numpy.int32 )
                    outdata *= (1 - dstMask)
                    outdata += dstMask*missingValue

                # fill in dstData
                exec('dstData' + slce + ' = outdata')

    def getDstGrid(self):
        """
        Return the destination grid, may be different from the dst grid provided 
        to the constructor due to padding and/or domain decomposition
        @return local grid on this processor
        """
        return self.tool.getDstGrid()
