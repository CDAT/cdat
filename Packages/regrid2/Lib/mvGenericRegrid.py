"""
Generic Regrid class.

This code is provided with the hope that it will be useful.
No guarantee is provided whatsoever. Use at your own risk.

David Kindig and Alex Pletzer, Tech-X Corp. (2012)
"""
import operator
import numpy
import string

import regrid2
import re
from distarray import MultiArrayIter

def guessPeriodicity(srcBounds):
    """
    Guess if a src grid is periodic
    @param srcBounds the nodal src set of coordinates
    @return 1 if periodic, warp around, 0 otherwise
    """
    res = 0
    if srcBounds is not None:
        res = 1
        # assume longitude to be the last coordinate
        lonsb = srcBounds[-1]
        nlon = lonsb.shape[-1]
        dlon = (lonsb.max() - lonsb.min()) / float(nlon)
        tol = 1.e-2 * dlon
        if abs( (lonsb[..., -1] - 360.0 - lonsb[..., 0]).sum()/float(lonsb.size) ) > tol:
            # looks like a regional model
            res = 0
    return res

class GenericRegrid:
    """
    Generic Regrid class.
    """
    def __init__(self, srcGrid, dstGrid,
                 dtype,
                 regridMethod,
                 regridTool,
                 srcGridMask = None, srcBounds = None, srcGridAreas = None,
                 dstGridMask = None, dstBounds = None, dstGridAreas = None,
                 **args):
        """
        Constructor.
        @param srcGrid list of numpy arrays, source horizontal coordinates
        @param dstGrid list of numpy arrays, destination horizontal coordinate
        @param dtype numpy data type for src/dst data
        @param regridMethod linear (bi, tri,...) default or conservative
        @param regridTool currently either 'libcf' or 'esmf'
        @param srcGridMask array of same shape as srcGrid
        @param srcBounds list of numpy arrays of same shape as srcGrid
        @param srcGridAreas array of same shape as srcGrid
        @param dstGridMask array of same shape as dstGrid
        @param dstBounds list of numpy arrays of same shape as dstGrid
        @param dstGridAreas array of same shape as dstGrid
        @param **args additional arguments to be passed to the
                      specific tool
                      'libcf': mkCyclic={True, False}, handleCut={True,False}
                      'esmf': periodicity={0,1}, coordSys={'deg', 'cart'}, ...
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
                 srcGridMask = srcGridMask,
                 srcBounds = srcBounds,
                 **args)
        elif re.search('esm', regridTool.lower()):
            # ESMF
            staggerLoc = args.get('staggerLoc', 'center')
            if args.has_key('staggerLoc'):
                del args['staggerLoc']
            periodicity = args.get('periodicity', 
                                   guessPeriodicity(srcBounds))
            if args.has_key('periodicity'):
                del args['periodicity']
            coordSys = args.get('coordSys', 'deg')
            if args.has_key('coordSys'):
                del args['coordSys']

            # Get the shapes
            self.srcGridShape = srcGrid[0].shape
            self.dstGridShape = dstGrid[0].shape
            self.hasSrcBounds = False
            self.hasDstBounds = False
            if srcBounds is not None:
                self.hasSrcBounds = True
            if dstBounds is not None:
                self.hasDstBounds = True
            self.srcGridAreasShape = None
            self.dstGridAreasShape = None
            if srcGridAreas is not None: 
                self.srcGridAreasShape = srcGridAreas[0].shape
            if dstGridAreas is not None:
                self.dstGridAreasShape = dstGridAreas[0].shape

            # Initialize
            self.tool = regrid2.ESMFRegrid(self.srcGridShape, self.dstGridShape,
                  dtype = dtype,
                  regridMethod = regridMethod,
                  staggerLoc = staggerLoc,
                  periodicity = periodicity,
                  coordSys = coordSys,
                  hasSrcBounds = self.hasSrcBounds,
                  hasDstBounds = self.hasDstBounds,
                  srcGridAreasShape= self.srcGridAreasShape,
                  dstGridAreasShape = self.dstGridAreasShape,
                  **args)
            self.tool.setCoords(srcGrid, dstGrid,
                  srcGridMask = srcGridMask,
                  srcBounds = srcBounds,
                  srcGridAreas = srcGridAreas,
                  dstGridMask = dstGridMask,
                  dstBounds = dstBounds,
                  dstGridAreas = dstGridAreas,
                  globalIndexing = True,
                  **args)
        else:
            msg = """mvGenericRegrid.__init__: ERROR unrecognized tool %s,
valid choices are: 'libcf', 'esmf'"""% regridTool
            raise regrid2.RegridError, msg

    def computeWeights(self, **args):
        """
        Compute Weights
        """
        self.tool.computeWeights(**args)

    def apply(self, srcData, dstData,
              rootPe = None,
              missingValue = None,
              **args):
        """
        Regrid source to destination
        @param srcData array (input)
        @param dstData array (output)
        @param rootPe if other than None, then results will be MPI
                      gathered
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

            self.tool.apply(srcData, dstData, rootPe = rootPe, 
                            globalIndexing = True, **args)

            # adjust for masking
            if missingValue is not None:
                srcDataMaskFloat[:] = (srcData == missingValue)
                # interpolate mask
                self.tool.apply(srcDataMaskFloat, dstDataMaskFloat,  
                                rootPe = rootPe, globalIndexing = True, **args)
                if re.search('conserv', self.regridMethod.lower(), re.I):
                    dstMask = numpy.array((dstDataMaskFloat == 1), numpy.int32)
                else:
                    dstMask = numpy.array((dstDataMaskFloat > 0), numpy.int32)
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
                self.tool.apply(indata, outdata, rootPe = rootPe, 
                                globalIndexing = True, **args)

                # adjust for masking
                if missingValue is not None:
                    srcDataMaskFloat[:] = (indata == missingValue)
                    # interpolate mask
                    self.tool.apply(srcDataMaskFloat, dstDataMaskFloat,
                                    rootPe = rootPe, globalIndexing = True, **args)
                    if re.search('conserv', self.regridMethod.lower(), re.I):
                        # cell interpolation
                        dstMask = numpy.array((dstDataMaskFloat == 1), numpy.int32)
                    else:
                        # nodal interpolation
                        dstMask = numpy.array((dstDataMaskFloat > 0), numpy.int32)
                    outdata *= (1 - dstMask)
                    outdata += dstMask*missingValue

                # fill in dstData
                exec('dstData' + slce + ' = outdata')

    def getDstGrid(self):
        """
        Return the destination grid, may be different from the dst grid provided
        to the constructor due to domain decomposition
        @return local grid on this processor
        """
        return self.tool.getDstGrid()

    def fillInDiagnosticData(self, diag, rootPe = None):
        """
        Fill in diagnostic data
        @param diag a dictionary whose entries, if present, will be filled
                    entries are tool dependent
        @param rootPe root processor where data should be gathered (or
                      None if local areas are to be returned)
        """
        self.tool.fillInDiagnosticData(diag, rootPe = rootPe)

