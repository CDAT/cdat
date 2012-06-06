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
import re
import numpy
try:
    from mpi4py import MPI
except:
    pass

class ESMFRegrid(GenericRegrid):
    """
    Regrid class for ESMF
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
        self.pe = 0
        try:
            self.pe = MPI.COMM_WORLD.Get_rank()
        except:
            pass

        coordSys = ESMP.ESMP_COORDSYS_SPH_DEG

        # These are exact
        for arg in args.keys():
            if arg == 'coordSys': 
                if re.search('cart', args[arg].lower()): 
                    coordSys = ESMP.ESMP_COORDSYS_CART
                if re.search('deg', args[arg].lower()):
                    coordSys = ESMP.ESMP_COORDSYS_SPH_DEG
                if re.search('rad', args[arg].lower()):
                    coordSys = ESMP.ESMP_COORDSYS_SPH_RAD
            elif arg == 'periodicity': periodicity = args[arg]
            elif arg == 'staggerloc': 
                if re.search('center', args[arg].lower()):
                    self.staggerloc = ESMP.ESMP_STAGGERLOC_CENTER
                if re.search('corner', args[arg].lower()):
                    self.staggerloc = ESMP.ESMP_STAGGERLOC_CORNER
            elif arg == 'unMappedAction': 
                if re.search('error', args[arg].lower()):
                    self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_ERROR
                if re.search('ignore', args[arg].lower()):
                    self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
            elif arg == 'regridMethod': 
                if re.search('cons', args[arg].lower()):
                    self.regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE
                if re.search('line', args[arg].lower()):
                    self.regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR
                if re.search('patch', args[arg].lower()):
                    self.regridMethod = ESMP.ESMP_REGRIDMETHOD_PATCH
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
            self.srcMaskValues = numpy.array([1],dtype = numpy.int32)

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
            self.dstMaskValues = numpy.array([1],dtype = numpy.int32)
        if dstBounds is not None:
            # Coords are CENTER (cell) based, bounds are CORNER (node) based
            if self.staggerloc != ESMP.ESMP_STAGGERLOC_CORNER:
                self.dstGrid.setCoords(dstBounds, 
                                   staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
            elif self.staggerloc == ESMP.ESMP_STAGGERLOC_CORNER:
                string = "If the stagger location is nodal, can't set the bounds"
                raise RegridError, string

        # Dummy fields for computing the interpolation weights
        sDV = srcGrid[0][:] * 0.0
        dDV = dstGrid[0][:] * -99.0
        self.srcDummyFld = esmf.EsmfStructField(self.srcGrid, 'srcDummyFld', 
                                           sDV, 
                                           staggerloc = self.staggerloc)
        self.dstDummyFld = esmf.EsmfStructField(self.dstGrid, 'dstDummyFld', dDV, 
                                           staggerloc = self.staggerloc)

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

        self.regridObj = esmf.EsmfRegrid(self.srcDummyFld, self.dstDummyFld,
                                  srcFrac = self.srcFrac, 
                                  dstFrac = self.dstFrac,
                                  srcMaskValues = self.srcMaskValues,
                                  dstMaskValues = self.dstMaskValues,
                                  regridMethod = regridMethod,
                                  unMappedAction = unMappedAction)

        if regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            self.srcFrac = esmf.EsmfStructField(self.srcGrid, 'srcFrac',
                                       self.srcDummyFld,
                                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
            self.dstFrac = esmf.EsmfStructField(self.dstGrid, 'dstFrac',
                                       self.dstDummyFld,
                                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
                                        
        # Compute the weights  
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            self.srcAreas = self.regridObj.getSrcAreas(rootPe = rootPe)
            self.dstAreas = self.regridObj.getDstAreas(rootPe = rootPe)
            self.srcFractions = self.regridObj.getSrcAreaFractions(rootPe = rootPe)
            self.dstFractions = self.regridObj.getDstAreaFractions(rootPe = rootPe)

    def apply(self, srcData, dstData, srcDataMask = None, **args):
        """
        Regrid source to destination
        @param srcData array
        @param dstData array
        @param srcDataMask array
        @param rootPe set to None if dstData is locally filled. Other values
                      will gather dstData on processor rootPe
        """

        rootPe = 0

        srcName = 'srcData'
        dstName = 'dstData'
        if hasattr(srcData, 'id'): 
            srcName = 'src_%s' % srcData.id
            dstName = 'dst_%s' % srcData.id

        self.srcVar = esmf.EsmfStructField(self.srcGrid, srcName, srcData, 
                                      staggerloc = self.staggerloc)
        self.dstVar = esmf.EsmfStructField(self.dstGrid, dstName, dstData,
                                    staggerloc = self.staggerloc)

        if hasattr(srcData, 'missing_value'):
            mask = numpy.array(srcData == srcData.missing_value, 
                               dtype = numpy.int32)
            self.mask = esmf.EsmfStructField(self.srcGrid, 'mask', mask,
                                    staggerloc = self.staggerloc)
            #newField = 

        self.regridObj(self.srcVar, self.dstVar)

        if 'rootPe' in args: rootPe = args['rootPe']
        print 'ESMF rootPe', rootPe

        if rootPe is None:
            slab = self.dstGrid.getLocalSlab()
            dstData[slab] = self.dstVar.getData(rootPe = rootPe)
        else:
            data = self.dstVar.getData(rootPe = rootPe)
            if self.pe == rootPe:
                dstData[:] = data

