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
        @return index set
        @note no checks are performed to ensure that the returned big index is valid
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
        @note no checks are performed to ensure that the returned indices are valid
        """
        return reduce(operator.add, [self.dimProd[i]*indices[i] \
                                         for i in range(self.ndims)], 0)

    def reset(self):
        """
        Reset big index
        """
        self.big_index = -1

    def getDims(self):
        """
        Get the axis dimensions
        @return list
        """
        return self.dims

    def isBigIndexValid(self, bigIndex):
        """
        Test if big index is valid
        """
        return bigIndex < self.ntot and bigIndex >= 0

    def areIndicesValid(self, inds):
        """
        Test if indices are valid
        """
        return reduce(operator.and_, [inds[d] < self.dims[d] \
                                         for d in range(self.ndims)], True)
        
######################################################################

def test(rowMajor):
    dims = (2, 3, 4)
    print 'row major: dims = ', dims
    for it in MultiArrayIter( (2, 3, 4), rowMajor = rowMajor):
        inds = it.getIndices()
        bi = it.getBigIndex()
        print inds, bi
        assert( it.getBigIndexFromIndices(inds) == it.getBigIndex() )
        inds2 = it.getIndicesFromBigIndex(bi)
        assert( reduce(operator.and_, \
                           [inds2[d] == inds[d] for d in range(it.ndims)], True ) )
        assert( it.isBigIndexValid(bi) )
        assert( it.areIndicesValid(inds) )

    # check validity
    assert( not it.isBigIndexValid(-1) )
    assert( not it.isBigIndexValid( it.ntot ) )
    assert( not it.areIndicesValid( (2, 2, 3) ) )
    assert( not it.areIndicesValid( (1, 3, 3) ) )
    assert( not it.areIndicesValid( (1, 2, 4) ) )

if __name__ == '__main__':
    test(rowMajor = True)
    test(rowMajor = False)
