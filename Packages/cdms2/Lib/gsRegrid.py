#/usr/bin/env python

"""
Regridding of curvilinear structured grids
Alex Pletzer and Dave Kindig, Tech-X (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""


# standard python includes
from re import search, sub
from ctypes import c_double, c_float, c_int, c_char_p, CDLL, byref, POINTER
import operator
import sys
import copy

import numpy

# libcf
try:
    from pycf import libCFConfig, __path__
except:
    raise ImportError, 'Error: could not import pycf'

LIBCFDIR  = __path__[0] + "/libcf"
#LIBCFDIR  = "/home/pletzer/software/libcf-debug/lib/libcf"
#LIBCFDIR  = "/home/pletzer/software/libcf-opt/lib/libcf"
#LIBCFDIR  = "/home/pletzer/software/libcf-debug-logging/lib/libcf"

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
    @return new list of coordinates
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
            raise CDMSError, "ERROR in %s: funky mixture of axes and curvilinear coords %s" \
                % (__FILE__, str([x.shape for x in coords]))
    return coords, dims

class Regrid:

    def __init__(self, src_grid, dst_grid):
        """
        Constructor
        
        @param src_grid source grid, a list of [x, y, ...] coordinates
        @param dst_grid destination grid, a list of [x, y, ...] coordinates
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

        # Open the shaped library
        for sosuffix in '.so', '.dylib', '.dll', '.DLL', '.a':
            self.lib = CDLL(LIBCFDIR + sosuffix)
            if self.lib:
                break
        if self.lib == None:
            raise CDMSError, "ERROR in %s: could not open shared library %s" \
                (__FILE__, LIBCFDIR)
        
        # Number of space dimensions
        self.ndims = len(src_grid)
        if len(dst_grid) != self.ndims:
            raise CDMSError, "ERROR in %s: len(dst_grid) = %d != %d" \
                (__FILE__, len(dst_grid), self.ndims)

        if self.ndims <= 0:
            raise CDMSError, "ERROR in %s: must have at least one dimension, ndims = %d" \
                (__FILE__, self.ndims)

        # Convert src_grid/dst_grid to curvilinear grid, if need be
        if self.ndims > 1:
            src_grid, src_dims = makeCurvilinear(src_grid)
            dst_grid, dst_dims = makeCurvilinear(dst_grid)

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
        c_double_p = POINTER(c_double)
        save = 0
        standard_name = ""
        units = ""
        coordid = c_int(-1)
        for i in range(self.ndims):
            data =  numpy.array( src_grid[i], numpy.float64 )
            self.src_coords.append( data )
            dataPtr = data.ctypes.data_as(c_double_p)
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
            dataPtr = data.ctypes.data_as(c_double_p)
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
                                 coord_periodicity.ctypes.data_as(c_double_p))
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
            raise CDMSError, "ERROR in %s: len(lo) = %d != %d" % \
                (__FILE__, len(lo), self.ndims)

        if len(hi) != self.ndims:
            raise CDMSError, "ERROR in %s: len(hi) = %d != %d" % \
                (__FILE__, len(hi), self.ndims)

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
        c_intmask = mask.ctypes.data_as(POINTER(c_int))
        status = self.lib.nccf_set_grid_validmask(self.regridid, c_intmask)

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
        # Check 
        if reduce(operator.iand, [src_data.shape[i] == self.src_dims[i] \
                                 for i in range(self.ndims)]) == False:
            raise CDMSError, ("ERROR in %s: supplied src_data have wrong shape " \
                + "%s != %s") % (__FILE__, str(src_data.shape), \
                                     str(tuple([d for d in self.src_dims])))
        if reduce(operator.iand, [dst_data.shape[i] == self.dst_dims[i] \
                                 for i in range(self.ndims)]) == False:
            raise CDMSError, ("ERROR in %s: supplied dst_data have wrong shape " \
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
        status = self.lib.nccf_get_regrid_weights(self.regridid,
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
        coord_periodicity = float('inf') * numpy.ones((self.ndims),
                                  dtype = numpy.float32).ctypes.data_as(POINTER(c_double))
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
                                                   coord_periodicity, 
                                                   posPtr,
                                                   byref(niter), 
                                                   byref(tol),
                                                   adjustFunc,
                                                   resPtr,
                                                   hit_bounds)
        catchError(status, sys._getframe().f_lineno)
        return resPtr.contents.value, niter.value, tol.value

######################################################################

def testOuterProduct():
    
    # 2d
    x = numpy.array([1, 2, 3, 4])
    y = numpy.array([10, 20, 30])
    xx = getTensorProduct(x, 0, [len(x), len(y)])
    yy = getTensorProduct(y, 1, [len(x), len(y)])
    print xx
    print yy

    # 3d
    z = numpy.array([100, 200])
    print getTensorProduct(x, 0, [len(x), len(y), len(z)])
    print getTensorProduct(y, 1, [len(x), len(y), len(z)])
    print getTensorProduct(z, 2, [len(x), len(y), len(z)])

    #Mixed coordinates and axes

    print "\nCurvilinear"
    aa = makeCurvilinear([z, yy, xx])
    print len(aa)
    for g in aa: 
        print
        print g.shape
        print g


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

    rg = Regrid([src_x, src_y, src_z], 
                [dst_x, dst_y, dst_z])
#    rg = Regrid([src_x, src_y], 
#                [dst_x, dst_y])

    kk = numpy.array([0.0, 0.0, 0.0])
    indices = rg._findIndices(numpy.array([1.5, 18.0, 140.0]), 
                              20, 1.e-2, kk)

    rg.computeWeights(10, 1.e-3)
    nvalid = rg.getNumValid()
    ndstpts = rg.getNumDstPoints()
    print 'nvalid = ', nvalid, ' ndstpts = ', ndstpts

    # Get the weights
    inds, weights = rg.getIndicesAndWeights([3, 1])

    # data 
    src_coords = rg.getSrcGrid()
    dst_coords = rg.getDstGrid()
    #print 'src_coords = ', src_coords
    #print 'dst_coords = ', dst_coords
    src_data = numpy.array( func1(src_coords), numpy.float32 )
    dst_data = -numpy.ones( dst_coords[0].shape, numpy.float32 )

    # regrid    
    rg.apply(src_data, dst_data)

    # check
    error = numpy.sum(abs(dst_data - func1(dst_coords)))
    #print dst_data
    #print func(dst_coords)
    print 'error = ', error
        

if __name__ == '__main__': 
    #testOuterProduct()
    test()
    
