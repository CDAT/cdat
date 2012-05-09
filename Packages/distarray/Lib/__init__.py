"""
Distributed array
"""
__all__ = ["mvDistArray", "mvGhostedDistArray",]
from mvDistArray import DistArray, zeros, ones, array
from mvGhostedDistArray import GhostedDistArray
