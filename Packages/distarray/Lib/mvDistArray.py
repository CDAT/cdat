#/usr/bin/env python

"""
Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.
"""

__version__ = "$Id: $"

# standard imports
import copy

# external dependencies
import numpy
from mpi4py import MPI

class DistArray(numpy.ndarray):

    """
    Distributed array. Each process owns data and can expose a subset 
    of the data to other processes. These are known as windows. Any 
    number of windows can be exposed and the data of windows can be 
    overlapping. Any process can access exposed windows from any other
    process. This relies on MPI-2 one-sided Get communication.
    """

    def __init__(self, shape, dtype):
        """
        Constructor
        @param shape tuple of dimensions
        @param dtype numpy type
        """
        self.comm = MPI.COMM_WORLD # default communicator
        self.windows = {}          # winID: {'slice': slce,
                                   #         'dataSrc': dataSrc,
                                   #         'dataDst': dataDst,
                                   #         'window': window}

        self.rk = self.comm.Get_rank()
        self.sz = self.comm.Get_size()

        if dtype == numpy.float64:
            self.dtypeMPI = MPI.DOUBLE
        elif dtype == numpy.float32:
            self.dtypeMPI = MPI.FLOAT
        elif dtype == numpy.int32:
            self.dtypeMPI = MPI.INT32_T
        elif dtype == numpy.int16:
            self.dtypeMPI = MPI.INT16_T
        elif dtype == numpy.int8:
            self.dtypeMPI = MPI.INT8_T
        else:
            raise NotImplementedError

    def setComm(self, comm):
        """
        Set communicator
        @param comm communicator
        """
        self.comm = comm
        self.rk = self.comm.Get_rank()
        self.sz = self.comm.Get_size()

    def expose(self, slce, winID):
        """
        Collective operation to expose a sub-set of data
        @param slce tuple of slice objects
        @param winID the data window ID
        """
        # buffer for source data
        dataSrc = float('inf') * numpy.ones(self[slce].shape, self.dtype) 
        # buffer for destination data
        dataDst = float('inf') * numpy.ones(self[slce].shape, self.dtype)
        self.windows[winID] = {
            'slice': slce,
            'dataSrc': dataSrc,
            'dataDst': dataDst,
            'window': MPI.Win.Create(dataSrc, comm=self.comm),
            }

    def get(self, pe, winID):
        """
        Access remote data (collective operation)
        @param pe remote processing element
        @param winID remote window
        @return array
        """
        iw = self.windows[winID]
        slce = iw['slice']
        dataSrc = iw['dataSrc']
        dataDst = iw['dataDst']

        # copy src data into buffer
        dataSrc[...] = self[slce]

        win = iw['window']
        win.Fence()
        win.Get( [dataDst, self.dtypeMPI], pe )
        win.Fence()

        return dataDst

    def free(self):
        """
        Must be called to free all exposed windows
        """
        for iw in self.windows:
            self.windows[iw]['window'].Free()
            
######################################################################

def test():

    comm = MPI.COMM_WORLD
    rk = comm.Get_rank()
    sz = comm.Get_size()
    
    # create local data container
    n, m = 3, 4
    data = numpy.reshape(numpy.array( [rk*100.0 + i for i in range(n*m) ] ), (n,m))

    # create dist array
    da = DistArray(data.shape, data.dtype)
    
    # load the data
    da[:] = data

    # expose data to other pes
    da.expose( (slice(None, None,), slice(-1, None,)), winID='east' )

    # fetch data
    if rk > 0:
        daOtherEast = da.get(pe=rk-1, winID='east')
    else:
        daOtherEast = da.get(pe=sz-1, winID='east')

    # check
    daLocalEast = da[ da.windows['east']['slice'] ]
    diff = daLocalEast - daOtherEast
    if rk > 0:
        try:
            assert( numpy.all( diff == 100 ) )
            print '[%d]...OK' % rk
        except: 
            print '[%d] daLocalEast=%s\ndaOtherEast=%s' % (rk, str(daLocalEast), str(daOtherEast))
            print 'error: ', numpy.sum( diff - 100 )
    else:
        try:
            assert( numpy.all( diff == -100*(sz-1) ) )
            print '[%d]...OK' % rk
        except:
            print '[%d] daLocalEast=%s\ndaOtherEast=%s' % (rk, str(daLocalEast), str(daOtherEast))
            print 'error: ', numpy.sum( diff + 100*(sz-1) )

    # delete windows
    da.free()

if __name__ == '__main__': test()
