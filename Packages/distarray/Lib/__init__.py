"""
Distributed array
"""
__all__ = ["mvMultiArrayIter"]
from mvMultiArrayIter import MultiArrayIter 

# is mpi4py available?
hasMpi4py = True
try:
    from mpi4py import MPI
except:
    hasMpi4py = False

if hasMpi4py:
    from mvDistArray import DistArray, daZeros, daOnes, daArray
    from mvGhostedDistArray import GhostedDistArray, ghZeros, ghOnes, ghArray
    __all__ += ["mvDistArray", "mvGhostedDistArray"]
