#/usr/bin/env python

"""
Muti-dimensional domain decomposition.

This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.

Alex Pletzer, Tech-X Corp. (2012)
"""

__version__ = "0.9"

# standard modules
import operator
from mvMultiArrayIter import MultiArrayIter

def getPrimeFactors(n):
    """
    Get all the prime factor of given integer
    @param n integer
    @return list [1, ..., n]
    """
    lo = [1]
    n2 = n // 2
    k = 2
    while k <= n2:
        if (n // k)*k == n:
            lo.append(k)
        k += 1
    return lo + [n,]

class CubeDecomp:

    def __init__(self, nprocs, dims, rowMajor=True):
        """
        Constructor
        @param nprocs number of sub-domains
        @param dims list of global dimensions along each axis
        @param rowMajor True if row major, False if column major
        """
        self.ndims = len(dims)
        self.nprocs = nprocs
        self.globalDims = dims
        self.rowMajor = rowMajor
        self.decomp = [None for i in range(self.ndims)]
        self.proc2IndexSet = {}

    def getDecomp(self):
        """
        Compute optimal dedomposition, each sub-domain has the 
        same volume in index space. 
        @return list if successful, empty list if not successful
        """
        primeNumbers = [getPrimeFactors(d) for d in self.globalDims]
        ns = [len(pns) for pns in primeNumbers]
        validDecomps = []
        for it in MultiArrayIter(ns):
            inds = it.getIndices()
            decomp = [primeNumbers[d][inds[d]] for d in range(self.ndims)]
            if reduce(operator.mul, decomp, 1) == self.nprocs:
                validDecomps.append(decomp)

        if len(validDecomps) == 0:
            # no solution
            return []

        # find the optimal decomp among all valid decomps
        minCost = float('inf')
        bestDecomp = validDecomps[0]
        for decomp in validDecomps:
            sizes = [self.globalDims[d]//decomp[d] for d in range(self.ndims)]
            volume = reduce(operator.mul, sizes, 1)
            surface = 0
            for d in range(self.ndims):
                surface += 2*reduce(operator.mul, sizes[:d], 1) * \
                    reduce(operator.mul, sizes[d+1:], 1)
            cost = surface / float(volume)
            if cost < minCost:
                bestDecomp = decomp
                minCost = cost
        self.decomp = bestDecomp

        # fill in the proc to index set map
        procId = 0
        self.proc2IndexSet = {}
        numCellsPerProc = [self.globalDims[d]//self.decomp[d] for d in range(self.ndims)]
        for it in MultiArrayIter(self.decomp, rowMajor = self.rowMajor):
            nps = it.getIndices()
            self.proc2IndexSet[procId] = [slice(nps[d]*numCellsPerProc[d], \
                                               (nps[d] + 1)*numCellsPerProc[d]) \
                                              for d in range(self.ndims)]
            procId += 1
        
        return self.decomp

    def getSlab(self, procId):
        """
        Get the start/end indices for given processor rank
        @param procId processor rank
        @return list of slices
        """
        if self.proc2IndexSet:
            return self.proc2IndexSet[procId]
        else:
            return []
        
######################################################################

def test():
    dims = (46, 72)
    
    for nprocs in 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 18:
        d = CubeDecomp(nprocs = nprocs, dims = dims)
        print 'nprocs = ', nprocs, ' decomp: ', d.getDecomp()
        for procId in range(nprocs):
            print ('[%d] start/end indices: ' % procId), d.getSlab(procId)
    

    
if __name__ == '__main__':
    test()
