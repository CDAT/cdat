import numpy, copy
import cdms2
import regrid2
from regrid2 import RegridError
import re

from IPython.Shell import IPShellEmbed
ipshell = IPShellEmbed()

def setMaskDtype(mask):
    if mask.dtype != numpy.int32:
        mask = numpy.array(mask, numpy.int32)
    return mask

class Regridder:
    def __init__(self, inGrid, outGrid, mask = None, regridTool = "gsRegrid",
                 regridMethod = '', **args):
        """
        Constructor for regridding object. Currently just uses a 2-D object
        @param ingrid cdms2, ndarray variable
        @param outGrid cdms2, ndarray variable
        @param mask The mask
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
        rgTool = regridTool.lower()
        rgMeth = regridMethod.lower()
        self.regridTool = rgTool
        self.regridMethod = rgMeth
        self.outmask = None
        
        if re.match('regrid', rgTool, re.I):
            self.regridObj = regrid2.Horizontal(ingrid, outGrid)
            self.regridTool = 'regrid2'
        elif rgTool == 'scrip':
            pass
        elif re.match('esm', rgTool):
            self.regridTool = 'esmp'
        elif rgTool == 'gsregrid' or rgTool == 'libcf':
            # Prep in and out grid for gsRegrid!
            if cdms2.isGrid(inGrid):
                index = 0
                hasTime = False
                for axis in inGrid.getAxisList():
                    if axis == 'time':
                        timindex = index
                        self.hasTime = True
                srcRank = len(inGrid.getAxisList())
                if srcRank == 2:
                    self.srcGrid = [inGrid.getLatitude(), inGrid.getLongitude()]
                elif srcRank == 3 or srcRank == 4:
                    if self.hasTime and srcRank == 3:
                        self.srcGrid = [inGrid.getLatitude(), inGrid.getLongitude()]
                    else:
                        self.srcGrid = [inGrid.getLevel(), 
                                        inGrid.getLatitude(), 
                                        inGrid.getLongitude()]
            elif isinstance(inGrid, list):
                srcRank = len(inGrid)
                self.srcGrid = inGrid
            else:
                raise RegridError, 'inGrid must be a list of coordinates or a cdms2 grid'
            self.order = 'yx'

            if cdms2.isGrid(outGrid):
                dstRank = len(outGrid.getAxisList())
                hasTime = False
                for axis in outGrid.getAxisList():
                    if axis == 'time':
                        timindex = index
                        hasTime = True
                if dstRank == 2:
                    self.dstGrid = [outGrid.getLatitude(), outGrid.getLongitude()]
                if dstRank == 3 or dstRank == 4:
                    if hasTime and srcRank == 3:
                        self.dstGrid = [outGrid.getLatitude(), outGrid.getLongitude()]
                    else:
                        self.dstGrid = [outGrid.getLevel(), 
                                        outGrid.getLatitude(), 
                                        outGrid.getLongitude()]
            elif isinstance(outGrid, list):
                dstRank = len(outGrid)
                self.dstGrid = outGrid 
            else:
                raise RegridError, 'outGrid must be a list of coordinates or a cdms2 grid'
            self.outGrid = outGrid

            if dstRank != srcRank:
                raise RegridError, 'outGrid rank (%d) != inGrid rank (%d)' % \
                                   (dstRank, srcRank)

            # Create the regrid object
            ro = regrid2.gsRegrid.Regrid(self.srcGrid, self.dstGrid)

            # Set the mask
            if mask is not None:
                if len(mask.shape) > 3:
                    raise RegridError, \
                           'Ranks greater than three are not supported at'
                self.mask = setMaskDtype(mask)
                ro.setValidMask(self.mask)

            # Compute the weights
            nitermax = 20
            nlat = self.dstGrid[0].shape[0]
            tolpos = 0.01 * 180. / (nlat-1)
            if 'nitermax' in args.keys():
                nitermax = args['nitermax']
            if 'tolpos' in args.keys():
                tolpos = args['tolpos']
            ro.computeWeights(nitermax = nitermax, 
                              tolpos   = tolpos)
            self.regridTool = 'gsregrid'
        else:
            raise RegridError, ''' Regrid tools are: regrid2,   
                  (gsRegrid or libcf), 
                  (esmf or esmp)'''
        
        self.regridObj = ro

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
            attrs = copy.copy(inData.attributes)
            varid = inData.id
            axislist = list(map(lambda x: x[0].clone(), inData.getDomain()))
            inputIsVariable = 1
            if 'order' not in args.keys():
                order = inData.getOrder()
            #this expects contiguous arrays
            if isinstance(inData, TransientVariable) and inData.iscontiguous() is False:
                inData = inData.ascontiguous()
        else:
            inputIsVariable = 0
        
        missing = None
        if 'missing' in args.keys():
            missing = args['missing']

        if self.regridTool == 'regrid2':
            result = self.regridObj(inData, args)
        elif self.regridTool == 'gsregrid':
            # The data has a mask and the mask has not been set previously
            
            if (not self.regridObj.maskSet) and inData.mask is not None:
                print 'WARNING: Ignoring mask'

            outVar = self.regridObj(inData)
            # Correct the shape of output weights
            amskout = numpy.ma.zeros(outVar.shape, numpy.bool8)
            if inputIsVariable == 1:
                if inData.fill_value is not None:
                    amskout = 1 - (outVar == inData.fill_value)

            # Set the missing data mask of the output array, if any.
            hasMissing = not numpy.ma.alltrue(numpy.ma.ravel(amskout))
            if hasMissing:
                slabMask = numpy.ma.where(numpy.ma.equal(amskout, 0), 1, 0)
            else:
                slabMask = None

            # Combine missing data mask and output grid mask
            # Note: slabMask and outmask are Boolean here
            if self.outmask is not None:
                outmask = numpy.logical_not(numpy.resize(self.outmask, outshape))
                if hasMissing:
                    outmask = numpy.ma.logical_or(outmask, slabMask)
            else:
                outmask = slabMask

            # Create the result TransientVariable (if input inData is an AbstractVariable)
            # or masked array
            if inputIsVariable==1:
                if isinstance(self.outGrid, list):
                    if len(self.outGrid) == 2:
                        index = 0
                    else:
                        index = 1
                    if not cdms2.isGrid(self.outGrid[index]):
                        from cdms2.coord import TransientAxis2D as T2D
                        lats = T2D(self.regridObj.dst_coords[index], id = 'lat')
                        lons = T2D(self.regridObj.dst_coords[index+1], id = 'lon')
                    grid = cdms2.hgrid.TransientCurveGrid(lats, lons, id = 'CurveGrid')
                ipshell() 
                if inData.rank() == 2: 
                    axisList = grid.getAxisList()
                elif inData.rank() == 3 or inData.rank() == 4:
                    if self.hasTime and inData.rank() == 3:
                        self.srcGrid = [inGrid.getLatitude(), inGrid.getLongitude()]
                    else:
                        self.srcGrid = [inGrid.getLevel(), 
                                        inGrid.getLatitude(), 
                                        inGrid.getLongitude()]
                elif inData.rank() == 3 and not self.hasTime:
                    axisList = [self.outGrid.getLevel()] + axisList

                result = cdms2.createVariable(outVar, mask = outmask, 
                                              fill_value = inData.fill_value,
                                              axes = axisList, 
                                              grid = grid, 
                                              attributes = attrs, id = varid)
            else:
                result = numpy.ma.masked_array(outVar, mask = outmask, fill_value = missing)

        elif self.regridTool == 'esmp':
            pass
        elif self.regridTool == 'scrip':
            pass  
        

        return result
         
