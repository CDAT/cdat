#/usr/bin/env python

"""
Copyright (c) 2008, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.
"""

__version__ = "$Id: $"

from mvDistArray import DistArray

class GhostedDistArray(DistArray):

    def getSlab(self, dim, slce):
        """
        Get slab. A slab is a multi-dimensional slice extending in
        all directions except dim where slce applies
        @param dim dimension where the slce applies (0=first index, 1=2nd index...)
        @param slce slice object along dimension dim
        @return slab
        """
        shape = self.shape
        ndim = len(shape)
        
        slab = [ slice(0, shape[i]) for i in range(dim) ] \
                    + [slce] + \
                  [ slice(0, shape[i]) for i in range(dim+1, ndim) ]
        return slab

    def setGhostWidth(self, ghostWidth=1):
        """
        Set the ghost slab width and expose windows
        @param ghostWidth a number typically > 0
        """

        shape = self.shape
        # expose each window to other PE domains
        ndim = len(shape)
        for dim in range(ndim):
            for drect in (-1, 1):
                # the window id uniquely specifies the
                # location of the window. we use 0's to indicate
                # a slab extending over the entire length for a
                # given direction, a 1 represents a layer of
                # thickness ghostWidth on the high index side,
                # -1 on the low index side.
                winId = tuple( [0 for i in range(dim) ] \
                               + [drect] + \
                               [0 for i in range(dim+1, ndim) ] )
                
                slce = slice(0, ghostWidth)
                if drect == 1:
                    slce = slice(shape[dim] - ghostWidth, shape[dim])
        
                slab = self.getSlab(dim, slce)
                
                # expose MPI window
                self.expose(slab, winId)

######################################################################
    
def test():

    from mpi4py import MPI
    import numpy

    comm = MPI.COMM_WORLD
    rk = comm.Get_rank()
    sz = comm.Get_size()

    # create local data container
    n, m = 2, 3
    data = numpy.reshape(numpy.array( [rk*100.0 + i for i in range(n*m) ] ),
                         (n,m))

    # create dist array
    da = GhostedDistArray(shape=data.shape, dtype=data.dtype)

    # load the data on each PE
    da[:] = data
    print da

    # set the number of ghosts and export slabs
    da.setGhostWidth(1)
    
    # this shows how one can access slabs 
    for pe in range(sz):
        winIndex = (-1, 0)
        print '[%d] %s slab belonging to %d is: \n' % (rk, str(winIndex), pe), \
              da.get(pe, winID=winIndex)
        winIndex = (+1, 0)
        print '[%d] %s slab belonging to %d is: \n' % (rk, str(winIndex), pe), \
              da.get(pe, winID=winIndex)
        winIndex = (0, -1)
        print '[%d] %s slab belonging to %d is: \n' % (rk, str(winIndex), pe), \
              da.get(pe, winID=winIndex)
        winIndex = (0, +1)
        print '[%d] %s slab belonging to %d is: \n' % (rk, str(winIndex), pe), \
              da.get(pe, winID=winIndex)

    # to keep mpi4py quite
    da.free()

if __name__ == '__main__': test()
