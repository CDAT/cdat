#/usr/bin/env python

"""
Multi-array iterator class.

This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.

Alex Pletzer, Tech-X Corp. (2012)
"""

__version__ = "0.9"

# standard modules
import operator

class MultiArrayIter:

    def __init__(self, dims, rowMajor=True):
        """
        Constructor
        @param dims list of dimensions along each axis
        @param rowMajor True if row major, False if column major
        """
        self.dims = dims
        self.ntot = reduce(operator.mul, self.dims, 1)
        self.ndims = len(self.dims)
        self.big_index = -1
        self.dimProd = [1 for i in range(self.ndims)]
        if rowMajor:
            # row major
            for i in range(self.ndims - 2, -1, -1):
                self.dimProd[i] =  self.dimProd[i + 1] * self.dims[i + 1]
        else:
            # column major
            for i in range(1, self.ndims):
                self.dimProd[i] =  self.dimProd[i - 1] * self.dims[i - 1]

    def __iter__(self):
        return self

    def next(self):
        if self.big_index < self.ntot - 1:
            self.big_index += 1
            return self
        else:
            raise StopIteration

    def getIndices(self):
        """
        @return current index set
        """
        return self.getIndicesFromBigIndex(self.big_index)

    def getBigIndex(self):
        """
        @return current big index
        """
        return self.big_index

    def getIndicesFromBigIndex(self, bigIndex):
        """
        Get index set from given big index
        @param bigIndex
        @retunr index set
        """
        indices = [0 for i in range(self.ndims)]
        for i in range(self.ndims):
            indices[i] = bigIndex // self.dimProd[i] % self.dims[i]
        return indices
        
    def getBigIndexFromIndices(self, indices):
        """
        Get the big index from a given set of indices
        @param indices 
        @return big index
        """
        return reduce(operator.add, [self.dimProd[i]*indices[i] \
                                         for i in range(self.ndims)], 0)

    def reset(self):
        """
        Reset big index
        """
        self.big_index = -1
        
######################################################################

def testRowMajor():
    dims = (2, 3, 4)
    print 'row major: dims = ', dims
    for it in MultiArrayIter( (2, 3, 4), rowMajor=True):
        inds = it.getIndices()
        print inds, it.getBigIndex()
        assert( it.getBigIndexFromIndices(inds) == it.getBigIndex() )

def testColMajor():
    dims = (2, 3, 4)
    print 'column major: dims = ', dims
    for it in MultiArrayIter( dims, rowMajor=False):
        inds = it.getIndices()
        print inds, it.getBigIndex()
        assert( it.getBigIndexFromIndices(inds) == it.getBigIndex() )
    
if __name__ == '__main__':
    testRowMajor()
    testColMajor()
