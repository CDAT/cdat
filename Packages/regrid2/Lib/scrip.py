## Automatically adapted for numpy.oldnumeric Aug 02, 2007 by 

import cdms2
import _scrip
from error import RegridError
import numpy

"""Regrid support for nonrectangular grids, based on the SCRIP package."""

class ScripRegridder:

    def __init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=None, sourceFrac=None, destFrac=None):
        self.outputGrid = outputGrid
        self.remapMatrix = remapMatrix
        self.sourceAddress = sourceAddress
        self.destAddress = destAddress
        self.inputGrid = inputGrid
        self.sourceFrac = sourceFrac
        self.destFrac = destFrac

    def __call__(self, input):

        import numpy.ma
        from cdms2 import isVariable
        from cdms2.tvariable import TransientVariable

        # If input is a variable, make it a TV
        if isVariable(input) and not isinstance(input, TransientVariable):
            input = input.subSlice()

        isvar = isinstance(input, TransientVariable)

        if isvar:
            domain = tuple(input.getAxisList())
            if self.inputGrid is not None:
                ingrid = self.inputGrid
            else:
                ingrid = input.getGrid()
            if ingrid is None:
                raise RegridError, "Input variable must have an associated grid."
            rank = len(ingrid.shape)
            gridsize = ingrid.size()
            outgridshape = self.outputGrid.shape

            # Check that the grid matches the last dimension(s) of input
            if input.shape[-rank:] != ingrid.shape:
                raise RegridError, 'Last dimensions of input array must match grid shape: %s'%`ingrid.shape`
            #this expects contiguous arrays
            if input.iscontiguous() is False:
                input = input.ascontiguous()

        else:
            rank = 1                    # If not a TV, last dimension is the 'cell' dimension
            gridsize = input.shape[-1]
            outgridshape = (reduce(lambda x,y: x*y, self.outputGrid.shape, 1),)
            
        # If input is an numpy.ma, make it Numeric
        if numpy.ma.isMaskedArray(input):
            input = input.filled()

        restoreShape = input.shape[:-rank]
        restoreLen = reduce(lambda x,y: x*y, restoreShape, 1)
        oldshape = input.shape
        newshape = (restoreLen, gridsize)
        input.shape = newshape

        # Regrid
        output = self.regrid(input)

        # Reshape output and restore input shape
        input.shape = oldshape
        outshape = restoreShape + outgridshape
        output.shape = outshape

        # If the input was a variable, so is the output
        if isvar:
            outdomain = domain[:-rank]+(self.outputGrid,)
            output = TransientVariable(output, axes=outdomain)

        return output

    def getOutputGrid(self):
        return self.outputGrid

    def getInputGrid(self):
        return self.inputGrid

    def getSourceFraction(self):
        return self.sourceFrac

    def getDestinationFraction(self):
        return self.destFrac

class ConservativeRegridder(ScripRegridder):
    """First-order conservative regrid. By default, the normalize option ="fracarea", and array 'normal'
    is not specified. If 'normal' is specified, it should be a one-dimensional array of the same length
    as the output grid size, with values:
      1.0 for normalize="fracarea",
      grid_frac for normalize="destarea", or
      grid_frac*grid_area for normalize="none".
    sourceArea is the area of the source grid cells
    destArea is the area of the destination grid cells
    """

    def __init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=None, sourceFrac=None, destFrac=None, normalize="fracarea", normal=None, sourceArea=None, destArea=None):
        if normalize not in ["fracarea", "destarea", "none"]:
            raise RegridError, "Invalid normalization option: %s"%normalize
        ScripRegridder.__init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=inputGrid, sourceFrac=sourceFrac, destFrac=destFrac)
        self.normalize = normalize
        self.normal = None
        self.sourceArea = sourceArea
        self.destArea = destArea

    def getSourceArea(self):
        return self.sourceArea

    def getDestinationArea(self):
        return self.destArea

    def regrid(self, input):
        if self.normal is None:
##             print "On input, num_links = %d"%(len(self.sourceAddress))
##             print "On input, nextra = %d"%(input.shape[0])
##             print "On input, ninput = %d"%(input.shape[1])
##             print "On input, noutput = %d"%(self.outputGrid.size())
##             print "On input, shape(input) = %s"%`input.shape`
##             print "On input, shape(output) = %s"%`self.outputGrid.shape`
##             print "On input, shape(remap_matrix) = %s"%`self.remapMatrix.shape`
##             print "On input, shape(src_address) = %s"%`self.sourceAddress.shape`
##             print "On input, shape(dst_address) = %s"%`self.destAddress.shape`
            result = _scrip.conserv_regrid(self.outputGrid.size(), input, self.remapMatrix, self.sourceAddress, self.destAddress)
        else:
            result = _scrip.conserv_regrid_normal(self.outputGrid.size(), input, self.remapMatrix, self.sourceAddress, self.destAddress, self.normal)
        return result

class BilinearRegridder(ScripRegridder):

    def __init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=None, sourceFrac=None, destFrac=None):
        ScripRegridder.__init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=inputGrid, sourceFrac=sourceFrac, destFrac=destFrac)

    def regrid(self, input):
        result = _scrip.bilinear_regrid(self.outputGrid.size(), input, self.remapMatrix, self.sourceAddress, self.destAddress)
        return result

class BicubicRegridder(ScripRegridder):
    """Bicubic regrid."""

    def __init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=None, sourceFrac=None, destFrac=None):
        ScripRegridder.__init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=inputGrid, sourceFrac=sourceFrac, destFrac=destFrac)

    def __call__(self, input, gradLat, gradLon, gradLatlon):
        """gradLat = df/di
        gradLon = df/dj
        gradLatlon = d(df)/(di)(dj)
        """

        import numpy.ma
        from cdms2 import isVariable
        from cdms2.tvariable import TransientVariable

        if (gradLat.shape != input.shape or
            gradLon.shape != input.shape or
            gradLatlon.shape != input.shape):
            raise RegridError, "All input arrays must have shape %s"%`input.shape`

        if (type(gradLat) is not type(input) or
            type(gradLon) is not type(input) or
            type(gradLatlon) is not type(input)):
            raise RegridError, "All input arrays must have type %s"%`type(input)`

        # If input is a variable, make it a TV
        if isVariable(input) and not isinstance(input, TransientVariable):
            input = input.subSlice()
            gradLat = gradLat.subSlice()
            gradLon = gradLon.subSlice()
            gradLatlon = gradLatlon.subSlice()

        isvar = isinstance(input, TransientVariable)

        if isvar:
            domain = tuple(input.getAxisList())
            if self.inputGrid is not None:
                ingrid = self.inputGrid
            else:
                ingrid = input.getGrid()
            if ingrid is None:
                raise RegridError, "Input variable must have an associated grid."
            rank = len(ingrid.shape)
            gridsize = ingrid.size()
            outgridshape = self.outputGrid.shape

            # Check that the grid matches the last dimension(s) of input
            if input.shape[-rank:] != ingrid.shape:
                raise RegridError, 'Last dimensions of input array must match grid shape: %s'%`ingrid.shape`

        else:
            rank = 1                    # If not a TV, last dimension is the 'cell' dimension
            gridsize = input.shape[-1]
            outgridshape = (reduce(lambda x,y: x*y, self.outputGrid.shape, 1),)
            
        # If input is an numpy.ma, make it Numeric
        if numpy.ma.isMaskedArray(input):
            input = input.filled()
            gradLat = gradLat.filled()
            gradLon = gradLon.filled()
            gradLatlon = gradLatlon.filled()

        restoreShape = input.shape[:-rank]
        restoreLen = reduce(lambda x,y: x*y, restoreShape, 1)
        oldshape = input.shape
        newshape = (restoreLen, gridsize)
        input.shape = newshape
        gradLat.shape = newshape
        gradLon.shape = newshape
        gradLatlon.shape = newshape

        # Regrid
        output = _scrip.bicubic_regrid(self.outputGrid.size(), input, self.remapMatrix, self.sourceAddress, self.destAddress, gradLat, gradLon, gradLatlon)


        # Reshape output and restore input shape
        input.shape = oldshape
        gradLat.shape = oldshape
        gradLon.shape = oldshape
        gradLatlon.shape = oldshape
        outshape = restoreShape + outgridshape
        output.shape = outshape

        # If the input was a variable, so is the output
        if isvar:
            outdomain = domain[:-rank]+(self.outputGrid,)
            output = TransientVariable(output, axes=outdomain)

        return output

class DistwgtRegridder(ScripRegridder):

    def __init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=None, sourceFrac=None, destFrac=None):
        ScripRegridder.__init__(self, outputGrid, remapMatrix, sourceAddress, destAddress, inputGrid=inputGrid, sourceFrac=sourceFrac, destFrac=destFrac)

    def regrid(self, input):
        result = _scrip.distwgt_regrid(self.outputGrid.size(), input, self.remapMatrix, self.sourceAddress, self.destAddress)
        return result


def readRegridder(fileobj, mapMethod=None, checkGrid=1):
    """Read a regridder from an open fileobj.
    mapMethod is one of "conservative", "bilinear", "bicubic", or "distwgt". If unspecified, it defaults to the method defined in the file.
    If 'checkGrid' is 1 (default), the grid cells are checked for convexity,
    and 'repaired' if necessary.
    """

    if isinstance(fileobj,str):
        fileobj = cdms2.open(fileobj)
    elif not isinstance(fileobj,cdms2.dataset.CdmsFile):
        raise RegridError,"fileobj arguments must be a cdms2 file or a string pointing to a file"
    
    if mapMethod is None:
        mapString = fileobj.map_method.strip().lower()
        if mapString[0:12]=="conservative":
            mapMethod = "conservative"
        elif mapString[0:8]=="bilinear":
            mapMethod = "bilinear"
        elif mapString[0:7]=="bicubic":
            mapMethod = "bicubic"
        elif mapString[0:8]=="distance" or mapString[0:7]=="distwgt":
            mapMethod = "distwgt"
        else:
            raise RegridError, "Unrecognized map method: %s"%mapString

    convention = 'SCRIP'
    if fileobj.variables.keys().count('S'):
        convention = 'NCAR'
    if convention == 'SCRIP':
        remapMatrix = fileobj('remap_matrix').filled()
        srcAddress = fileobj('src_address').filled()
        dstAddress = fileobj('dst_address').filled()
        srcfrac = fileobj('src_grid_frac')
        dstfrac = fileobj('dst_grid_frac')
    else:
        remapMatrix = fileobj('S').filled()
        srcAddress = fileobj('col').filled()
        dstAddress = fileobj('row').filled()
        srcfrac = fileobj('frac_a')
        dstfrac = fileobj('frac_b')
    ingrid = fileobj.readScripGrid(whichGrid="source", checkGrid=checkGrid)
    outgrid = fileobj.readScripGrid(whichGrid="destination", checkGrid=checkGrid)

    if mapMethod=="conservative":
        if convention == 'SCRIP':
            srcarea = fileobj('src_grid_area')
            dstarea = fileobj('dst_grid_area')
        else: #NCAR stuff
            if "S2" in fileobj.variables.keys():
                remapMatrix=fileobj("S2")
                sh = list(remapMatrix.shape)
                if len(sh)==2 and sh[-1]==2:
                    sh[-1]=1
                    S=fileobj("S").filled()
                    S.shape=sh
                    remapMatrix = numpy.concatenate((S,remapMatrix),axis=1)
            srcarea = fileobj('area_a')
            dstarea = fileobj('area_b')
        regridder = ConservativeRegridder(outgrid, remapMatrix,srcAddress, dstAddress, inputGrid=ingrid, sourceFrac=srcfrac, destFrac=dstfrac, sourceArea=srcarea, destArea=dstarea)
    elif mapMethod=="bilinear":
        regridder = BilinearRegridder(outgrid, remapMatrix,srcAddress, dstAddress, inputGrid=ingrid, sourceFrac=srcfrac, destFrac=dstfrac)
    elif mapMethod=="bicubic":
        regridder = BicubicRegridder(outgrid, remapMatrix,srcAddress, dstAddress, inputGrid=ingrid, sourceFrac=srcfrac, destFrac=dstfrac)
    elif mapMethod=="distwgt":
        regridder = DistwgtRegridder(outgrid, remapMatrix,srcAddress, dstAddress, inputGrid=ingrid, sourceFrac=srcfrac, destFrac=dstfrac)
    else:
        raise RegridError, "Unrecognized map method: %s"%mapMethod

    return regridder

