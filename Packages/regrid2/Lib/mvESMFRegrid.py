"""
ESMF regridding class

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""
import ESMP
from regrid2 import esmf
from regrid2 import GenericRegrid

class ESMFRegrid(GenericRegrid):
    """
    """
    def __init__(self, srcGrid, dstGrid, 
                 srcMask = None, srcBounds = None, srcAreas = None,
                 dstMask = None, dstBounds = None, dstAreas = None,
                 **args):
        """
        Constructor
        @param srcGrid list [x, y,[z]] of arrays in (z,y,x) order
        @param dstGrid list [x, y,[z]] of arrays in (z,y,x) order

        These are all properties of the grid and can be retrieved 
        using esmf methods:
        @param srcMask list [x, y,[z]] of arrays in (z,y,x) order
        @param srcBounds list [x, y,[z]] of arrays in (z,y,x) order
        @param srcAreas list [x, y,[z]] of arrays in (z,y,x) order
        @param dstMask list [x, y,[z]] of arrays in (z,y,x) order
        @param dstBounds list [x, y,[z]] of arrays in (z,y,x) order
        @param dstAreas list [x, y,[z]] of arrays in (z,y,x) order
        @param **args staggerloc, coordSys, periodicity, regridMethod, 
                      unMappedAction, srcMaskValues, dstMaskValues
        """

        self.srcGrid = None
        self.dstGrid = None
        self.srcShape = srcGrid[0].shape
        self.dstShape = dstGrid[0].shape
        self.staggerloc = ESMP.ESMP_STAGGERLOC_CENTER
        self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
        self.regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR
        self.srcMaskValues = None
        self.dstMaskValues = None
        self.srcFrac = None
        self.dstFrac = None

        coordSys = ESMP.ESMP_COORDSYS_SPH_DEG
        periodicity = 0

        # These are exact
        for arg in args.keys():
            if arg == 'coordSys': coordSys = args[arg]
            elif arg == 'periodicity': periodicity = args[arg]
            elif arg == 'staggerloc': self.staggerloc = args[arg]
            elif arg == 'unMappedAction': self.unMappedAction = args[arg]
            elif arg == 'regridMethod': self.regridMethod = args[arg]
            elif arg == 'srcMaskValues': self.srcMaskValues = args[arg]
            elif arg == 'dstMaskValues': self.dstMaskValues = args[arg]
            else:
                string = 'Unrecognized ESMP argument %s' % arg
                raise RegridError, string

        # Source Grid
        self.srcGrid = esmf.EsmfStructGrid(self.srcShape, 
                                coordSys = coordSys,
                                periodicity = periodicity)
        self.srcGrid.setCoords(srcGrid, staggerloc = self.staggerloc)
        if srcMask is not None:
            self.srcGrid.setCellMask(srcMask)

        if srcBounds is not None:
            # Coords are CENTER (cell) based, bounds are CORNER (node) based
            if self.staggerloc != ESMP.ESMP_STAGGERLOC_CORNER:
                self.srcGrid.setCoords(srcBounds, 
                                   staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
            elif self.staggerloc == ESMP.ESMP_STAGGERLOC_CORNER:
                string = "If the stagger location is nodal, can't set the bounds"
                raise RegridError, string

        # Destination Grid
        self.dstGrid = esmf.EsmfStructGrid(dstGrid[0].shape, 
                                coordSys = coordSys,
                                periodicity = periodicity)
        self.dstGrid.setCoords(dstGrid, staggerloc = self.staggerloc)
        if dstMask is not None:
            self.dstGrid.setCellMask(dstMask)
        if dstBounds is not None:
            # Coords are CENTER (cell) based, bounds are CORNER (node) based
            if self.staggerloc != ESMP.ESMP_STAGGERLOC_CORNER:
                self.dstGrid.setCoords(dstBounds, 
                                   staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
            elif self.staggerloc == ESMP.ESMP_STAGGERLOC_CORNER:
                string = "If the stagger location is nodal, can't set the bounds"
                raise RegridError, string

    def computeWeights(self, regridMethod = None, unMappedAction = None,
                  srcMaskValues = None, dstMaskValues = None):
        """
        Compute Weights
        @param regridMethod Bilinear, Conseravative or Patch
        @param unMappedAction ESMP Behavior on errors
        @param srcMaskValues array of values to be masked out e.g. [1] (Default)
        @param dstMaskValues array of values to be masked out e.g. [1] (Default)
        """
        
        if regridMethod is None: regridMethod = self.regridMethod
        if unMappedAction is None: unMappedAction = self.unMappedAction

        # Create dummy variables for use in generating the weights
        #sDV = self.srcGrid.getCoords(0, staggerloc = self.staggerloc) * 1.0
        #dDV = self.dstGrid.getCoords(0, staggerloc = self.staggerloc) * -1.0
        #self.srcDummyVar = esmf.EsmfStructField(self.srcGrid, 'srcDummyVar', sDV, 
        #                                   staggerloc = self.staggerloc)
        #self.dstDummyVar = esmf.EsmfStructField(self.dstGrid, 'dstDummyVar', dDV, 
        #                                   staggerloc = self.staggerloc)

#        self.regridObj = esmf.EsmfRegrid(self.srcDummyVar, self.dstDummyVar,
#                                  srcFrac = self.srcFrac, 
#                                  dstFrac = self.dstFrac,
#                                  srcMaskValues = self.srcMaskValues,
#                                  dstMaskValues = self.dstMaskValues,
#                                  regridMethod = regridMethod,
#                                  unMappedAction = unMappedAction)
#
        if regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            self.srcFrac = esmf.EsmfStructField(self.srcGrid, 'srcFrac',
                                       self.srcDummyVar,
                                       staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
            self.dstFrac = esmf.EsmfStructField(self.dstGrid, 'dstFrac',
                                       self.dstDummyVar,
                                       staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
                                        
        # Compute the weights  

    def apply(self, srcData, dstData, **args):
        """
        Regrid source to destination
        @param srcData array
        @param dstData array
        """
        regridMethod = None
        unMappedAction = None
        self.srcMaskValues = None
        self.dstMaskValues = None
        self.srcFrac = None
        self.dstFrac = None

        srcName = 'srcData'
        dstName = 'dstData'
        if hasattr(srcData, 'id'): 
            srcName = 'src_%s' % srcData.id
            dstName = 'dst_%s' % srcData.id

        rootPe = None
        if 'rootPe' in args.keys(): rootPe = args['rootPe']

        self.srcVar = esmf.EsmfStructField(self.srcGrid, srcName, srcData, 
                                      staggerloc = self.staggerloc)
        self.dstVar = esmf.EsmfStructField(self.dstGrid, dstName, dstData, 
                                      staggerloc = self.staggerloc)

        self.regridObj = esmf.EsmfRegrid(self.srcVar, self.dstVar,
                                  srcFrac = self.srcFrac, 
                                  dstFrac = self.dstFrac,
                                  srcMaskValues = self.srcMaskValues,
                                  dstMaskValues = self.dstMaskValues,
                                  regridMethod = regridMethod,
                                  unMappedAction = unMappedAction)


        print 'BEG with regridding'
        self.regridObj(self.srcVar, self.dstVar)
        print 'DONE with regridding'
        print 
        dstData = self.dstVar.getData(rootPe = rootPe) 
        print dstData.reshape(self.dstShape).shape
        print

#        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
#            self.srcAreas = self.regridObj.getSrcAreas(rootPe = rootPe)
#            self.dstAreas = self.regridObj.getDstAreas(rootPe = rootPe)
#            self.srcFractions = self.regridObj.getSrcAreaFractions(rootPe = rootPe)
#            self.dstFractions = self.regridObj.getDstAreaFractions(rootPe = rootPe)

