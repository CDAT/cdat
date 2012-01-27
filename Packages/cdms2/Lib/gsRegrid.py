#/usr/bin/env python

"""
Regridding of curvilinear structured grids
Alex Pletzer and Dave Kindig, Tech-X (2011)
This code is provided with the hope that it will be useful.
No guarantee is provided whatsoever. Use at your own risk.
"""


# standard python includes
from re import search, sub
from ctypes import c_double, c_float, c_int, \
    c_char_p, CDLL, byref, POINTER
import operator
import sys
import copy
import numpy

C_DOUBLE_P = POINTER(c_double)

# libcf
try:
    from pycf import libCFConfig, __path__
except:
    raise ImportError, 'Error: could not import pycf'

LIBCFDIR  = __path__[0] + "/pylibcf"

try:
    from error import CDMSError
except:
    from cdms2.error import CDMSError

__FILE__ = sys._getframe().f_code.co_filename

def catchError(status, lineno):
    if status != 0:
        raise CDMSError, "ERROR in %s: status = %d at line %d" \
            % (__FILE__, status, lineno)

def getNetCDFFillValue(dtype):
    """
    Get the NetCDF fill value
    @param dtype numpy data type, e.g. numpy.float32
    """
    if dtype == numpy.float64:
        return libCFConfig.NC_FILL_DOUBLE
    elif dtype == numpy.float32:
        return libCFConfig.NC_FILL_FLOAT
    elif dtype == numpy.int32:
        return libCFConfig.NC_FILL_INT
    elif dtype == numpy.int16:
        return libCFConfig.NC_FILL_SHORT
    elif dtype == numpy.int8:
        return libCFConfig.NC_FILL_BYTE
    else:
        raise CDMSError, "ERROR in %s: invalid type %s" \
            % (__FILE__, str(dtype))

def getTensorProduct(axis, dim, dims):
    """
    Convert an axis into a curvilinear coordinate by applying
    a tensor product
    @param axis 1D array of coordinates
    @param dim dimensional index of the above coordinate
    @param dims sizes of all coordinates
    @return coordinate values obtained by tensor product
    """
    return numpy.outer(numpy.outer( numpy.ones(dims[:dim], axis.dtype), axis),
                      numpy.ones(dims[dim+1:], axis.dtype)).reshape(dims)

def makeCurvilinear(coords):
    """
    Turn a mixture of axes and curvilinear coordinates into
    full curvilinear coordinates
    @param coords list of coordinates
    @return new list of coordinates and associated dimensions
    """
    ndims = len(coords)

    count1DAxes = 0
    dims = []
    for i in range(ndims):
        coord = coords[i]
        if len(coord.shape) == 1:
            # axis
            dims.append( len(coord) )
            count1DAxes += 1
        elif len(coord.shape) == ndims:
            # fully curvilinear
            dims.append( coord.shape[i] )
        else:
            # assumption: all 1D axes preceed curvilinear
            # coordinates!!!
            dims.append( coord.shape[i - count1DAxes] )

    for i in range(ndims):
        nd = len(coords[i].shape)
        if nd == ndims:
            # already in curvilinear form, keep as is
            pass
        elif nd == 1:
            # it's an axis
            coords[i] = getTensorProduct(coords[i], i, dims)
        elif ndims == 3 and nd == 2 and i > 0:
            # assume leading coordinate is an axis
            o1 = numpy.ones( (len(coords[0]),), coords[i].dtype )
            coords[i] = numpy.outer(o1, coords[i]).reshape(dims)
        else:
            raise CDMSError, \
                "ERROR in %s: funky mixture of axes and curvilinear coords %s" \
                % (__FILE__, str([x.shape for x in coords]))
    return coords, dims

def makeCoordsCyclic(coords, dims):
    """
    Make coordinates cyclic
    @params coords input coordinates
    @params dims input dimensions
    @return new, extended coordinates such that the longitudes cover the sphere
            and new dimensions
    """
    # assume lon is the last coordinate!!

    # check if already extended
    eps = 1.e-3

    # some models already overlap
    diff1 = abs(coords[-1][...,-2] - coords[-1][...,0])
    diff2 = abs(coords[-1][...,-2] - coords[-1][...,0] - 360.0)
    diff3 = abs(coords[-1][...,-2] - coords[-1][...,0] + 360.0)
    adiff = numpy.sum(numpy.minimum(diff1, numpy.minimum(diff2, diff3))) \
        / float(dims[-1])
    if adiff < eps:
        # cyclic, return input coordinates unchanged
        return coords, dims

    # some models are already periodic
    diff1 = abs(coords[-1][...,-1] - coords[-1][...,0])
    diff2 = abs(coords[-1][...,-1] - coords[-1][...,0] - 360.0)
    diff3 = abs(coords[-1][...,-1] - coords[-1][...,0] + 360.0)
    adiff = numpy.sum(numpy.minimum(diff1, numpy.minimum(diff2, diff3))) \
        / float(dims[-1])
    if adiff < eps:
        # cyclic, return input coordinates unchanged
        return coords, dims

    # make cyclic by appending a column to the coordinates
    newCoords = []
    newDims = list(copy.copy(dims))
    newDims[-1] += 1 # append to the right
    for i in range(len(coords)):
        newCoords.append( numpy.zeros( newDims, coords[i].dtype ) )
        newCoords[i][..., 0:-1] = coords[i][...]
        newCoords[i][...,   -1] = coords[i][...,  0]

    # add modulo term, want deltas ~ order of dlon otherwise add
    # or subtract a periodicity length
    nlon = dims[-1]
    dlon = 360.0 / float(nlon) # average resolution
    tol = 360.0 - min(5, nlon)*dlon
    mask1 = (newCoords[-1][..., -1] - newCoords[-1][..., -2] < -tol)
    mask2 = (newCoords[-1][..., -1] - newCoords[-1][..., -2] > +tol)
    newCoords[-1][..., -1] += 360.0*mask1
    newCoords[-1][..., -1] -= 360.0*mask2

    return newCoords, newDims

def checkForCoordCut(coords, dims):
    """
    Look for a cut in a coordinate system (e.g. tri-polar grid)
    Assume latitude is next to last coordinate and longitude is last coordinate!!!

    @params coords input coordinates
    @params dims input dimensions
    @return True for cut found
            False for no cut
    """

    # Assume latitude is next to last coordinate and longitude is last coordinate!!!

    ndims = len(dims)
    if ndims < 2:
        # print 'no cut: dims < 2'
        return False
    if len(coords[-2].shape) < 2:
        # Is the 'lat' coordinate an axis?
        return False

    nlat, nlon = dims[-2], dims[-1]
    lat = coords[-2]
    eps = 1.e-7

    # Check to see if the top row has already be dealt with by the modeling
    # agency. Last row is repeated in reverse.

    topRow = coords[-2][..., nlat-1, :]
    revTop = coords[-2][..., nlat-1, ::-1]
    nextRow = coords[-2][..., nlat-2, :]  # Reverse nextRow
    diffs = abs(revTop - nextRow)

    # If already accounted for all diffs are 0.
    if numpy.all(diffs < eps):
        # print "no cut: reversed"
        return False

    # Lon of max latitude -- Looking for a rotated pole
    maxLats = numpy.where(abs(lat - numpy.max(lat)) < eps)
    inTopRow = False
    if len(maxLats[0] > 0): inTopRow = numpy.all(maxLats[-2] == nlat-1)
    if not inTopRow:
        # The max lats are not in the top row. The cut may already be handled
        # print 'no cut: max lat not in top row.' + \
        #         'Either: it is a funky grid or rotated pole'
        return False

    # Only in top row.
    maxLatInd = lat[..., nlat-1, :].argmax()
    maxLonInd = lat[..., maxLatInd].argmax()
    rowOfMaxLat = lat[..., maxLonInd, :]
    diffs = rowOfMaxLat - topRow

    if diffs.max() != 0:
        # Rotated Pole
        # print "no cut: rotated pole"
        return False

    # Find locale minima.
    # A rotated pole grid has only one minimum. A tripolar grid should
    # have two, though they may not be at the same latitude

    minInds = numpy.where(abs(topRow - topRow.min()) < eps)
    if len(minInds[0]) > 2:
        # Account for the end points matching
        # Multiple minima in the top row
        return True

    # Now if we have an offset tri-pole. The extra poles are not at the same
    # latitude
    minCount = 0
    firstInd = topRow.argmin()
    diffs = numpy.diff(topRow)
    if firstInd == 0:
        if revTop.argmin() == 0:
            if topRow[firstInd] == revTop[0]: minCount += 1
    else:
        minCount += 1
        # Look for next Minima
    index = firstInd + 1
    while diffs[index] > 0:
        index += 1
        if index == nlon: break
    nextIndex = topRow[index+1:].argmin() + index+1
    if nextIndex != index+1 and nextIndex != nlon-1: minCount += 1
    if minCount == 1:
        # print "no cut: one pole"
        return False

    return True

def handleCoordsCut(coords, dims, bounds):
    """
    Generate connectivity across a cut. e.g. from a tri-polar grid.
    Assume latitude is next to last coordinate and longitude is last coordinate!!!

    @params coords input coordinates list of ndims
    @params dims input dimensions
    @params bounds boundaries for each coordinate
    @return extended coordinates such that there is an extra row containing
            connectivity information across the cut
    """

    # Assume latitude is next to last coordinate and longitude is 
    # last coordinate!!!

    dims = coords[-2].shape
    ndims = len(dims)
    nlat, nlon = dims[-2], dims[-1]

    epsExp = 3
    eps = 10**(-1*epsExp)

    # Add row to top with connectivity information. This means rearranging
    # the top row
    def getIndices(array, nlon, newI):
        """
        Find indices where a cell edge matches for two cells
        @param array Array of booleans
        @param nlon number of longitudes
        @param newI index row with connectivity to be updated
        @return new coordinates, new dimensions, index row
        """
        for i in range(len(array)):
            # An edge
            if len(numpy.where(array[i, :]==True)[0]) >= 2:
                if newI[i] < 0: newI[i] = (nlon-1) - i
                if newI[(nlon-1)-i] < 0: newI[(nlon-1)-i] = i

    lonb = bounds[-1][..., nlat-1, ...]
    latb = bounds[-2][..., nlat-1, ...]

    # Assume mkCyclic == True
    newI = numpy.arange(nlon-1, -1, -1)-1
    newI[nlon-1] = 0 #  Complete the rotation

    # Build new coordinate array and adjust dims
    newCoords = []
    newDims = list(copy.copy(dims))
    newDims[-2] += 1

    for i in range(len(coords)):
        nD = len(dims)
        newCoords.append( numpy.zeros( newDims, coords[i].dtype ) )
        newCoords[i][..., 0:dims[-2], :] = coords[i][...]
        newCoords[i][..., dims[-2], :] = coords[i][..., dims[-2]-1, newI]

    return newCoords, newDims, newI

class Regrid:

    def __init__(self, src_grid, dst_grid, src_bounds=None, mkCyclic=False,
                 handleCut=False, diagnostics=False):
        """
        Constructor

        @param src_grid source grid, a list of [x, y, ...] coordinates
        @param dst_grid destination grid, a list of [x, y, ...] coordinates
        @param src_bounds list of cell bounding coordinates (to be used when 
                          handling a cut in coordinates)
        @param mkCyclic Add a column to the right side of the grid to complete
               a cyclic grid
        @param handleCut Add a row to the top of grid to handle a cut for
               grids such as the tri-polar grid
        @param diagnostics print diagnostic messages
        @note the grid coordinates can either be axes (rectilinear grid) or
              n-dimensional for curvilinear grids. Rectilinear grids will
              be converted to curvilinear grids.
        """
        self.regridid = c_int(-1)
        self.src_gridid = c_int(-1)
        self.dst_gridid = c_int(-1)
        self.ndims = 0
        self.src_dims = []
        self.dst_dims = []
        self.src_coords = []
        self.dst_coords = []
        self.lib = None
        self.extendedGrid = False
        self.handleCut = False
        self.dst_Index = []
        self.diagnostics = diagnostics

        # Open the shaped library
        for sosuffix in '.dylib', '.dll', '.DLL', '.so', '.a':
            try:
                self.lib = CDLL(LIBCFDIR + sosuffix)
                break
            except:
                pass
        if self.lib == None:
            raise CDMSError, "ERROR in %s: could not open shared library %s" \
                % (__FILE__, LIBCFDIR)

        # Number of space dimensions
        self.ndims = len(src_grid)
        if len(dst_grid) != self.ndims:
            raise CDMSError, "ERROR in %s: len(dst_grid) = %d != %d" \
                % (__FILE__, len(dst_grid), self.ndims)

        if self.ndims <= 0:
            raise CDMSError, \
                "ERROR in %s: must have at least one dimension, ndims = %d" \
                % (__FILE__, self.ndims)

        # Convert src_grid/dst_grid to curvilinear grid, if need be
        if self.ndims > 1:
            src_grid, src_dims = makeCurvilinear(src_grid)
            dst_grid, dst_dims = makeCurvilinear(dst_grid)

        # Make sure coordinates wrap around if mkCyclic is True
        if mkCyclic:
            src_gridNew, src_dimsNew = makeCoordsCyclic(src_grid, src_dims)
            if self.diagnostics:
                aa, bb = str(src_dims), str(src_dimsNew)
                print '...  src_dims = %s, after making cyclic src_dimsNew = %s' \
                    % (aa, bb)
                for i in range(self.ndims):
                    print '...... src_gridNew[%d].shape = %s' \
                        % (i, str(src_gridNew[i].shape))
            # flag indicating that the grid was extended
            if reduce(lambda x, y:x+y, \
                          [src_dimsNew[i] - src_dims[i] \
                               for i in range(self.ndims)]) > 0:
                self.extendedGrid = True
            # reset
            src_grid = src_gridNew
            src_dims = src_dimsNew

        # Handle a cut in the coordinate system. Run after mkCyclic.
        # e.g. a tri-polar grid
        if handleCut and src_bounds is not None:
            # Test for the presence of a cut.
            isCut = checkForCoordCut(src_grid, src_dims)
            if isCut:
                # No cut
                src_gridNew, src_dimsNew, dst_Index = handleCoordsCut(src_grid,
                                                     src_dims, src_bounds)
                if dst_Index is not None:
                    self.handleCut = True
                    self.extendedGrid = self.extendedGrid
                else:
                    self.handleCut = False
                    self.extendedGrid = self.extendedGrid
                if self.diagnostics:
                    aa, bb = str(src_dims), str(src_dimsNew)
                    print '...  src_dims = %s, after making cyclic src_dimsNew = %s' \
                        % (aa, bb)
                src_grid = src_gridNew
                src_dims = src_dimsNew
                self.dst_Index = dst_Index

        self.src_dims = (c_int * self.ndims)()
        self.dst_dims = (c_int * self.ndims)()

        # Build coordinate objects
        src_dimnames = (c_char_p * self.ndims)()
        dst_dimnames = (c_char_p * self.ndims)()
        for i in range(self.ndims):
            src_dimnames[i] = 'src_n%d' % i
            dst_dimnames[i] = 'dst_n%d' % i
            self.src_dims[i] = src_dims[i]
            self.dst_dims[i] = dst_dims[i]
        self.src_coordids = (c_int * self.ndims)()
        self.dst_coordids = (c_int * self.ndims)()
        save = 0
        standard_name = ""
        units = ""
        coordid = c_int(-1)
        for i in range(self.ndims):
            data =  numpy.array( src_grid[i], numpy.float64 )
            self.src_coords.append( data )
            dataPtr = data.ctypes.data_as(C_DOUBLE_P)
            name = "src_coord%d" % i
            # assume [lev,] lat, lon ordering
            if i == self.ndims - 2:
                standard_name = 'latitude'
                units = 'degrees_north'
            elif i == self.ndims - 1:
                standard_name = 'longitude'
                units = 'degrees_east'
            status = self.lib.nccf_def_coord(self.ndims, self.src_dims,
                                             src_dimnames,
                                             dataPtr, save, name,
                                             standard_name, units,
                                             byref(coordid))
            catchError(status, sys._getframe().f_lineno)
            self.src_coordids[i] = coordid

            data =  numpy.array( dst_grid[i], numpy.float64 )
            self.dst_coords.append( data )
            dataPtr = data.ctypes.data_as(C_DOUBLE_P)
            name = "dst_coord%d" % i
            status = self.lib.nccf_def_coord(self.ndims, self.dst_dims,
                                             dst_dimnames,
                                             dataPtr, save, name,
                                             standard_name, units,
                                             byref(coordid))
            catchError(status, sys._getframe().f_lineno)
            self.dst_coordids[i] = coordid

        # Build grid objects
        status = self.lib.nccf_def_grid(self.src_coordids, "src_grid",
                                        byref(self.src_gridid))
        catchError(status, sys._getframe().f_lineno)

        status = self.lib.nccf_def_grid(self.dst_coordids, "dst_grid",
                                        byref(self.dst_gridid))
        catchError(status, sys._getframe().f_lineno)

        # Create regrid object
        status = self.lib.nccf_def_regrid(self.src_gridid, self.dst_gridid,
                                          byref(self.regridid))
        catchError(status, sys._getframe().f_lineno)

    def getPeriodicities(self):
        """
        Get the periodicity lengths of the coordinates
        @return numpy array, values inf indicate no periodicity
        """
        coord_periodicity = numpy.zeros( (self.ndims,), numpy.float64 )
        status = self.lib.nccf_inq_grid_periodicity(self.src_gridid,
                                 coord_periodicity.ctypes.data_as(C_DOUBLE_P))
        catchError(status, sys._getframe().f_lineno)
        return coord_periodicity

    def __del__(self):
        """
        Destructor, will be called automatically
        """
        status = self.lib.nccf_free_regrid(self.regridid)
        catchError(status, sys._getframe().f_lineno)

        status = self.lib.nccf_free_grid(self.src_gridid)
        catchError(status, sys._getframe().f_lineno)

        status = self.lib.nccf_free_grid(self.dst_gridid)
        catchError(status, sys._getframe().f_lineno)

        for i in range(self.ndims):

            status = self.lib.nccf_free_coord(self.src_coordids[i])
            catchError(status, sys._getframe().f_lineno)

            status = self.lib.nccf_free_coord(self.dst_coordids[i])
            catchError(status, sys._getframe().f_lineno)

    def addForbiddenBox(self, lo, hi):
        """
        Add a forbidden box, points falling inside the box will not
        be interpolated.
        @param lo inclusive lower set of indices
        @param hi inclusive upper set of indices
        """

        # Check lo and hi
        if len(lo) != self.ndims:
            raise CDMSError, "ERROR in %s: len(lo) = %d != %d" \
                % (__FILE__, len(lo), self.ndims)

        if len(hi) != self.ndims:
            raise CDMSError, "ERROR in %s: len(hi) = %d != %d" \
                % (__FILE__, len(hi), self.ndims)

        # Apply
        loIndices = (c_int * self.ndims)(tuple(lo))
        hiIndices = (c_int * self.ndims)(tuple(hi))
        status = self.lib.nccf_add_regrid_forbidden(self.regridid,
                                                    loIndices,
                                                    hiIndices)
        catchError(status, sys._getframe().f_lineno)

    def setValidMask(self, mask):
        """
        Set a mask for the grid
        @param mask an array of type char of size dims for the grid
        """
        # run some checks.
        if mask.dtype != numpy.int32:
            raise CDMSError, \
                "ERROR in %s: mask must be array of integers" \
                % (__FILE__,)

        # extend src data if grid was made cyclic and or had a cut accounted for
        newMask = self._extend(mask)
        c_intmask = newMask.ctypes.data_as(POINTER(c_int))
        status = self.lib.nccf_set_grid_validmask(self.src_gridid,
                                                  c_intmask)
        catchError(status, sys._getframe().f_lineno)

    def computeWeights(self, nitermax=100, tolpos=1.e-2):
        """
        Compute the the interpolation weights

        @param nitermax max number of iterations
        @param tolpos max tolerance when locating destination positions in
               index space
        """
        status = self.lib.nccf_compute_regrid_weights(self.regridid,
                                                      nitermax,
                                                      c_double(tolpos))
        catchError(status, sys._getframe().f_lineno)

    def apply(self, src_data, dst_data):
        """
        Apply interpolation
        @param src_data data on source grid
        @param dst_data data on destination grid
        @note destination coordinates falling outside the valid domain
              of src_data will not be interpoloted, the corresponding
              dst_data will not be touched.
        """
        # extend src data if grid was made cyclic and or had a cut accounted for
        src_data = self._extend(src_data)

        # Check
        if reduce(operator.iand, [src_data.shape[i] == self.src_dims[i] \
                                 for i in range(self.ndims)]) == False:
            raise CDMSError, ("ERROR in %s: supplied src_data have wrong shape " \
                                  + "%s != %s") % (__FILE__, str(src_data.shape), \
                                     str(tuple([d for d in self.src_dims])))
        if reduce(operator.iand, [dst_data.shape[i] == self.dst_dims[i] \
                                 for i in range(self.ndims)]) == False:
            raise CDMSError, ("ERROR ins: supplied dst_data have wrong shape " \
                + "%s != %s") % (__FILE__, str(dst_data.shape),
                                 str(self.dst_dims))

        # Create data objects
        src_dataid = c_int(-1)
        dst_dataid = c_int(-1)
        save = 0
        standard_name = ""
        units = ""
        time_dimname = ""

        status = self.lib.nccf_def_data(self.src_gridid, "src_data", \
                                        standard_name, units, time_dimname, \
                                            byref(src_dataid))
        catchError(status, sys._getframe().f_lineno)
        if src_data.dtype == numpy.float64:
            fill_value = c_double(libCFConfig.NC_FILL_DOUBLE)
            status = self.lib.nccf_set_data_double(src_dataid,
                                                   src_data.ctypes.data_as(POINTER(c_double)),
                                                   save, fill_value)
            catchError(status, sys._getframe().f_lineno)
        elif src_data.dtype == numpy.float32:
            fill_value = c_float(libCFConfig.NC_FILL_FLOAT)
            status = self.lib.nccf_set_data_float(src_dataid,
                                                  src_data.ctypes.data_as(POINTER(c_float)),
                                                  save, fill_value)
            catchError(status, sys._getframe().f_lineno)
        elif src_data.dtype == numpy.int32:
            fill_value = c_int(libCFConfig.NC_FILL_INT)
            status = self.lib.nccf_set_data_int(src_dataid,
                                                src_data.ctypes.data_as(POINTER(c_int)),
                                                save, fill_value)
            catchError(status, sys._getframe().f_lineno)
        else:
            raise CDMSError, "ERROR in %s: invalid src_data type = %s" \
                % (__FILE__, src_data.dtype)


        status = self.lib.nccf_def_data(self.dst_gridid, "dst_data", \
                                        standard_name, units, time_dimname, \
                                            byref(dst_dataid))
        catchError(status, sys._getframe().f_lineno)
        if dst_data.dtype == numpy.float64:
            fill_value = c_double(libCFConfig.NC_FILL_DOUBLE)
            status = self.lib.nccf_set_data_double(dst_dataid,
                                                   dst_data.ctypes.data_as(POINTER(c_double)),
                                                   save, fill_value)
            catchError(status, sys._getframe().f_lineno)
        elif dst_data.dtype == numpy.float32:
            fill_value = c_float(libCFConfig.NC_FILL_FLOAT)
            status = self.lib.nccf_set_data_float(dst_dataid,
                                                  dst_data.ctypes.data_as(POINTER(c_float)),
                                                  save, fill_value)
            catchError(status, sys._getframe().f_lineno)
        elif dst_data.dtype == numpy.int32:
            fill_value = c_int(libCFConfig.NC_FILL_INT)
            status = self.lib.nccf_set_data_int(dst_dataid,
                                                dst_data.ctypes.data_as(POINTER(c_int)),
                                                save, fill_value)
            catchError(status, sys._getframe().f_lineno)
        else:
            raise CDMSError, "ERROR in %s: invalid dst_data type = %s" \
                % (__FILE__, dst_data.dtype)

        # Now apply weights
        status = self.lib.nccf_apply_regrid(self.regridid, src_dataid, dst_dataid)
        catchError(status, sys._getframe().f_lineno)

        # Clean up
        status = self.lib.nccf_free_data(src_dataid)
        catchError(status, sys._getframe().f_lineno)
        status = self.lib.nccf_free_data(dst_dataid)
        catchError(status, sys._getframe().f_lineno)

    def __call__(self, src_data, dst_data):
        """
        Apply interpolation (synonymous to aply method)
        @param src_data data on source grid
        @param dst_data data on destination grid
        @note destination coordinates falling outside the valid domain
              of src_data will not be interpoloted, the corresponding
              dst_data will not be touched.
        """
        self.apply(src_data, dst_data)


    def getNumValid(self):
        """
        Return the number of valid destination points. Destination points
        falling outside the source domain, more gnerally, points which
        could not be located on the source grid, reduce the number of
        valid points.
        @return number of points
        """
        res = c_int(-1)
        status = self.lib.nccf_inq_regrid_nvalid(self.regridid,
                                                 byref(res))
        catchError(status, sys._getframe().f_lineno)
        return res.value

    def getNumDstPoints(self):
        """
        Return the number of points on the destination grid
        @return number of points
        """
        res = c_int(-1)
        status = self.lib.nccf_inq_regrid_ntargets(self.regridid,
                                                  byref(res))
        catchError(status, sys._getframe().f_lineno)
        return res.value

    def getSrcGrid(self):
        """
        Return the source grid
        @return grid
        """
        return self.src_coords

    def getDstGrid(self):
        """
        Return the destination grid
        @return grid
        """
        return self.dst_coords

    def getIndicesAndWeights(self, dst_indices):
        """
        Get the indices and weights for a single target location
        @param dst_indices index set on the target grid
        @return [index sets on original grid, weights]
        """
        dinds = numpy.array(dst_indices)
        sinds = (c_int * 2**self.ndims)()
        weights = numpy.zeros( (2**self.ndims,), numpy.float64 )
        status = self.lib.nccf_inq_regrid_weights(self.regridid,
                                                  dinds.ctypes.data_as(POINTER(c_double)),
                                                  sinds,
                                                  weights.ctypes.data_as(POINTER(c_double)))
        catchError(status, sys._getframe().f_lineno)
        # convert the flat indices to index sets
        ori_inds = []
        for i in range(2**self.ndims):
            inx = numpy.zeros( (self.ndims,), numpy.int32 )
            self.lib.nccf_get_multi_index(self.ndims, self.src_dims,
                                          sinds[i],
                                          inx.ctypes.data_as(POINTER(c_int)))
            ori_inds.append(inx)

        return ori_inds, weights

    def _extend(self, src_data):
        """
        Extend the data by padding a column and a row, depending on whether the
        grid was made cyclic and a fold was added or not
        @param src_data input source data
        @return extended source data (or source input data of no padding was applied)
        """

        # extended dimensions
        nlatX, nlonX = self.src_dims[-2], self.src_dims[-1]
        # original dimensions, before extension
        # assuming ..., lat, lon ordering
        nlat, nlon = src_data.shape[-2:]

        # no cut and no cyclic extension
        src_dataNew = src_data

        if self.handleCut or self.extendedGrid:
            # copy data into new, extended container
            src_dataNew = numpy.zeros(self.src_dims, src_data.dtype)
            # start filling in...
            src_dataNew[..., :nlat, :nlon] = src_data[...]

        if self.handleCut:
            # fill in polar cut (e.g. tripolar cut), top row
            # self.dst_Index[i] knows how to fold
            for i in range(nlon):
                src_dataNew[..., -1, i] = src_data[..., -2, self.dst_Index[i]]

        if self.extendedGrid:
            # make data periodic in longitudes
            src_dataNew[..., -1] = src_dataNew[..., 0]

        return src_dataNew

    def _findIndices(self, targetPos, nitermax, tolpos,
                     dindicesGuess):
        """
        Find the floating point indices
        @param targetPos numpy array of target positions
        @param nitermax max number of iterations
        @param tolpos max toelrance in positions
        @param dindicesGuess guess for the floating point indices
        @return indices, number of iterations, achieved tolerance
        """
        posPtr = targetPos.ctypes.data_as(POINTER(c_double))
        adjustFunc = None
        hit_bounds = numpy.zeros((self.ndims),
                                  dtype = int).ctypes.data_as(POINTER(c_int))
        # no periodicity
        coord_periodicity = float('inf') * numpy.ones((self.ndims), targetPos.dtype)
        coord_periodicity_ptr = coord_periodicity.ctypes.data_as(POINTER(c_double))
        res = copy.copy(dindicesGuess)
        resPtr = res.ctypes.data_as(POINTER(c_double))
        src_coords = (POINTER(c_double) * self.ndims)()
        niter = c_int(nitermax)
        tol = c_double(tolpos)
        for i in range(self.ndims):
            ptr = self.src_coords[i].ctypes.data_as(POINTER(c_double))
            src_coords[i] = ptr
        status = self.lib.nccf_find_indices_double(self.ndims,
                                                   self.src_dims,
                                                   src_coords,
                                                   coord_periodicity_ptr,
                                                   posPtr,
                                                   byref(niter),
                                                   byref(tol),
                                                   adjustFunc,
                                                   resPtr,
                                                   hit_bounds)
        catchError(status, sys._getframe().f_lineno)
        return resPtr.contents.value, niter.value, tol.value

######################################################################

def testMakeCyclic():

    y = numpy.array([-90.0 + i*30.0 for i in range(7)])
    x = numpy.array([(i+0.5)*60.0 for i in range(6)])
    yy = getTensorProduct(y, 0, [len(y), len(x)])
    xx = getTensorProduct(x, 1, [len(y), len(x)])
    coords = [yy, xx]
    dims = [len(y), len(x)]
    newCoords, newDims = makeCoordsCyclic(coords, dims)
#    print 'cyclic lats'
#    print newCoords[0]
#    print 'cyclic lons'
#    print newCoords[1]

def testHandleCut():

    # Need tripolar grid
    import cdms2
    filename = "data/so_Omon_GFDL-ESM2M_1pctCO2_r1i1p2_000101-000512_2timesteps.nc"
    f = cdms2.open(filename)
    if not f: return

    so = f.variables['so'][0, 0, :, :]
    if 'lon' in f.variables.keys():
        alllat = f.variables['lat']
        alllon = f.variables['lon']
    else:
        alllat = f.getAxis("lat").getData()
        alllon = f.getAxis("lon").getData()

    bounds = [f.variables['bounds_lon'][:].data,
              f.variables['bounds_lat'][:].data]
    coords = [alllat[:].data, alllon[:].data]
    dims = alllat.shape
    from matplotlib import pylab
    newCoords, newDims = makeCoordsCyclic(coords, dims)
    newCoords, newDims = handleCoordsCut(newCoords, newDims, bounds)

def testOuterProduct():

    # 2d
    x = numpy.array([1, 2, 3, 4])
    y = numpy.array([10, 20, 30])
    xx = getTensorProduct(x, 0, [len(x), len(y)])
    yy = getTensorProduct(y, 1, [len(x), len(y)])

    # 3d
    z = numpy.array([100, 200])

    #Mixed coordinates and axes

    aa = makeCurvilinear([z, yy, xx])
#    for g in aa:
#        print g


def test():

    def func1(coords):
        return coords[0]*coords[1] + coords[2]
    def func2(coords):
        return coords[0] * coords[1]

    # source grid, tensor product of axes
    src_x = numpy.array([1, 2, 3, 4, 5, 6])
    src_y = numpy.array([10, 20, 30, 40, 50])
    src_z = numpy.array([100, 200])

    # destination grid, product of axes
    dst_x = numpy.array([1.5, 2.0, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5])
    dst_y = numpy.array([15., 20., 25., 30., 40.])
    dst_z = numpy.array([120.0, 180.0, 240.])

    # regridding constructor
    rg = Regrid([src_x, src_y, src_z],
                [dst_x, dst_y, dst_z])
#    rg = Regrid([src_x, src_y],
#                [dst_x, dst_y])

    initialIndexGuess = numpy.array([0.0, 0.0, 0.0])
    indices = rg._findIndices(numpy.array([1.5, 18.0, 140.0]),
                              20, 1.e-2, initialIndexGuess)

    maxNumIters = 20
    posTol = 1.e-3
    rg.computeWeights(maxNumIters, posTol)

    # number of valid points (some destination points may fall 
    # outside the domain)
    nvalid = rg.getNumValid()

    # number of destination points
    ndstpts = rg.getNumDstPoints()
    print 'nvalid = ', nvalid, ' ndstpts = ', ndstpts

    # get the indices and weights for a single target location
    dst_indices = [4, 2, 1]
    inds, weights = rg.getIndicesAndWeights(dst_indices)
    print 'indices and weights are: ', inds, weights

    # data
    src_coords = rg.getSrcGrid()
    dst_coords = rg.getDstGrid()
    #print 'src_coords = ', src_coords
    #print 'dst_coords = ', dst_coords
    src_data = numpy.array( func1(src_coords), numpy.float32 )
    dst_data = -numpy.ones( dst_coords[0].shape, numpy.float32 )

    # regrid
    rg(src_data, dst_data)
    print 'after interp: dst_data = ', dst_data

    # check
    error = numpy.sum(abs(dst_data - func1(dst_coords)))
    #print dst_data
    #print func(dst_coords)
    print 'error = ', error

if __name__ == '__main__':
    #testOuterProduct()
    test()
    #testMakeCyclic()
    #testHandleCut()

