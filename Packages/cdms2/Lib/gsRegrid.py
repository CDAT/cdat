#/usr/bin/env python

"""
Regridding of curvilinear structured grids
Alex Pletzer and Dave Kindig, Tech-X (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""

# libcf
try:
    from pycf import libCFConfig, __path__
except:
    raise ImportError, 'Error: could not import pycf'

LIBCFDIR  = __path__[0] + "/libcf"

# standard python includes
from re import search, sub
from ctypes import c_double, c_char_p, c_int, CDLL, byref, POINTER

# numpy
import numpy

import sys
__FILE__ = sys._getframe().f_code.co_filename

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

        # Determine the source/destination sizes
        src_dims = src_grid[0].shape
        if len(src_dims) < self.ndims:
            src_dims = tuple([len(a) for a in src_grid])
        dst_dims = dst_grid[0].shape
        if len(dst_dims) < self.ndims:
            dst_dims = tuple([len(a) for a in src_grid])

        # Convert src_grid/dst_grid to curvilinear grid, if need be
        if self.ndims > 1:
            for i in range(self.ndims):
                if len(src_grid[i].shape) == 1:
                    src_grid[i] = getTensorProduct(src_grid[i], i, 
                                                   src_dims)
                if len(dst_grid[i].shape) == 1:
                    dst_grid[i] = getTensorProduct(dst_grid[i], i,
                                                   dst_dims)

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
        save = 1
        standard_name = ""
        units = ""
        coordid = c_int(-1)
        for i in range(self.ndims):
            data =  numpy.array( src_grid[i], numpy.float64 )
            dataPtr = data.ctypes.data_as(c_double_p)
            name = "src_coord%d" % i
            status = self.lib.nccf_def_coord(self.ndims, self.src_dims, 
                                             src_dimnames, 
                                             dataPtr, save, name, 
                                             standard_name, units, 
                                             byref(coordid))
            if status != 0:
                raise CDMSError, "ERROR in %s: status = %d at line %d" \
                    % (__FILE__, status, sys._getframe().f_lineno)
            self.src_coordids[i] = coordid

            data =  numpy.array( dst_grid[i], numpy.float64 )
            dataPtr = data.ctypes.data_as(c_double_p)
            name = "dst_coord%d" % i
            status = self.lib.nccf_def_coord(self.ndims, self.dst_dims, 
                                             dst_dimnames, 
                                             dataPtr, save, name, 
                                             standard_name, units, 
                                             byref(coordid))
            if status != 0:
                raise CDMSError, "ERROR in %s: status = %d at line %d" \
                    % (__FILE__, status, sys._getframe().f_lineno)
            self.dst_coordids[i] = coordid

        # Build grid objects
        status = self.lib.nccf_def_grid(self.src_coordids, "src_grid", 
                                        byref(self.src_gridid))
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)

        status = self.lib.nccf_def_grid(self.dst_coordids, "dst_grid", 
                                        byref(self.dst_gridid))
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)

        # Create regrid object
        status = self.lib.nccf_def_regrid(self.src_gridid, self.dst_gridid, 
                                          byref(self.regridid))
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)


    def __del__(self):
        """
        Destructor, will be called automatically
        """
        status = self.lib.nccf_free_regrid(self.regridid)
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)

        status = self.lib.nccf_free_grid(self.src_gridid)
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)
        
        status = self.lib.nccf_free_grid(self.dst_gridid)
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)

        for i in range(self.ndims):

            status = self.lib.nccf_free_coord(self.src_coordids[i])
            if status != 0:
                raise CDMSError, "ERROR in %s: status = %d at line %d" \
                    % (__FILE__, status, sys._getframe().f_lineno)

            status = self.lib.nccf_free_coord(self.dst_coordids[i])
            if status != 0:
                raise CDMSError, "ERROR in %s: status = %d at line %d" \
                    % (__FILE__, status, sys._getframe().f_lineno) 

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
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)

    def computeWeights(self, nitermax=100, tolpos=1.e-2, is_periodic=[]):
        """
        Compute the the interpolation weights
        
        @param nitermax max number of iterations
        @param tolpos max tolerance when locating destination positions in 
               index space
        @param is_periodic a list of bools to denote periodicity along
                           axes. A coordinate is said to be periodic iff
                           the last coordinate value matches the first.
        """
        # cast into a ctypes array of int's
        isPeriodic = (c_int * self.ndims)()
        for i in range(self.ndims):
            isPeriodic[i] = 0
        if is_periodic != []:
            if is_periodic[i]: 
                isPeriodic[i] = 1
        status = self.lib.nccf_compute_regrid_weights(self.regridid,
                                                      nitermax, 
                                                      c_double(tolpos),
                                                      isPeriodic)
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)
        

    def getNumValid(self):
        """
        Return the number of valid destination points. Destination points
        falling outside the source domain, more gnerally, points which 
        could not be located on the source grid, reduce the number of 
        valid points.
        @return number of valid points
        """
        res = c_int(-1)
        status = self.lib.nccf_inq_regrid_nvalid(self.regridid, 
                                                 byref(res))
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)
        return res.value

    def getNumDstPoints(self):
        """
        Return the number of points on the destination grid
        @return number of valid points
        """
        res = c_int(-1)
        status = self.lib.nccf_inq_regrid_ntargets(self.regridid, 
                                                  byref(res))
        if status != 0:
            raise CDMSError, "ERROR in %s: status = %d at line %d" \
                % (__FILE__, status, sys._getframe().f_lineno)
        return res.value

######################################################################

def testOuterProduct():
    
    # 2d
    x = numpy.array([1, 2, 3, 4])
    y = numpy.array([10, 20, 30])
    print getTensorProduct(x, 0, [len(x), len(y)])
    print getTensorProduct(y, 1, [len(x), len(y)])

    # 3d
    z = numpy.array([100, 200])
    print getTensorProduct(x, 0, [len(x), len(y), len(z)])
    print getTensorProduct(y, 1, [len(x), len(y), len(z)])
    print getTensorProduct(z, 2, [len(x), len(y), len(z)])

def test():
    
    # rectilinear grid
    src_x = numpy.array([1, 2, 3, 4])
    src_y = numpy.array([10, 20, 30])
    src_z = numpy.array([100, 200])

    # curvilinear grid
    dst_x = numpy.array([1.5, 2.0, 2.5, 3.5])
    dst_y = numpy.array([15., 20., 25.])
    dst_z = numpy.array([120.0, 180.0])

    rg = Regrid([src_x, src_y, src_z], 
                [dst_x, dst_y, dst_z])
    rg.computeWeights(10, 1.e-3, is_periodic=[False, False, False])
    nvalid = rg.getNumValid()
    ndstpts = rg.getNumDstPoints()
    print 'nvalid = ', nvalid, ' ndstpts = ', ndstpts

if __name__ == '__main__': 
    #testOuterProduct()
    test()
    
