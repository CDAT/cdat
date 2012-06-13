"""
LibCF regridding class

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""

from regrid2 import gsRegrid
from regrid2 import GenericRegrid

class LibCFRegrid(GenericRegrid):
    """
    """
    def __init__(self, srcGrid, dstGrid, srcGridMask = None, 
                 src_bounds = None, **args):
        """
        Constructor
        @param srcGrid array
        @param dstGrid array
        @param src_bounds cell boundaries
        @param **args keyword arguments, eg mkCyclic, handleCut, ...
                      to be passed to gsRegrid
        """
        mkCyclic = args.get('mkCyclic', False)
        handleCut = args.get('handleCut', False)
        self.regridObj = gsRegrid.Regrid(srcGrid, dstGrid, 
                                         src_bounds = src_bounds,
                                         mkCyclic=mkCyclic,
                                         handleCut=handleCut)
        if srcGridMask is not None: 
            self.regridObj.setMask(srcGridMask)

    def computeWeights(self, **args):
        """
        Compute interpolation weights
        @param **args arguments to be passed to gsRegrid, e.g. 
                      nitermax, tolpos, ...
        """
        nitermax = args.get('nitermax', 20)
        tolpos = args.get('tolpos', 0.01)
        self.regridObj.computeWeights(nitermax=nitermax, tolpos=tolpos)

    def apply(self, srcData, dstData, srcDataMask = None, **args):
        """
        Regrid source to destination
        @param srcData array (input)
        @param dstData array (output)
        """
        self.regridObj.apply(srcData, dstData)

    def getDstGrid(self):
        """
        Get the grid of the dst data (maybe larger than the 
        dst grid passed to the constructor due to column/row
        padding)
        @return shape
        """
        return self.regridObj.getDstGrid()
        


