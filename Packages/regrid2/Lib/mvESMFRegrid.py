"""
ESMF regridding class

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""
import re
import numpy
import string

import ESMP
from regrid2 import esmf
from regrid2 import RegridError
from regrid2 import GenericRegrid
from regrid2 import RegridError

try:
    from mpi4py import MPI
except:
    pass

class ESMFRegrid(GenericRegrid):
    """
    Regrid class for ESMF
    """
    def __init__(self, srcGrid, dstGrid, regridMethod = 'Linear',
                 srcGridMask = None, srcBounds = None, srcGridAreas = None,
                 dstGridMask = None, dstBounds = None, dstGridAreas = None,
                 **args):
        """
        Constructor
        @param srcGrid list [x, y,[z]] of arrays in (z,y,x) order
        @param dstGrid list [x, y,[z]] of arrays in (z,y,x) order

        These are all properties of the grid and can be retrieved 
        using esmf methods:
        @param srcGridMask list [x, y,[z]] of arrays in (z,y,x) order
        @param srcBounds list [x, y,[z]] of arrays in (z,y,x) order
        @param srcGridAreas list [x, y,[z]] of arrays in (z,y,x) order
        @param dstGridMask list [x, y,[z]] of arrays in (z,y,x) order
        @param dstBounds list [x, y,[z]] of arrays in (z,y,x) order
        @param dstGridAreas list [x, y,[z]] of arrays in (z,y,x) order
        @param **args staggerloc, coordSys, periodicity, regridMethod, 
                      unMappedAction, srcMaskValues, dstMaskValues
        """

        self.srcGrid = None
        self.dstGrid = None
        self.srcGridShape = srcGrid[0].shape
        self.dstGridShape = dstGrid[0].shape
        self.staggerloc = ESMP.ESMP_STAGGERLOC_CENTER
        self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
        self.regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR
        self.srcMaskValues = None
        self.dstMaskValues = None
        self.missing_value = 1.e+20
        self.srcFracFld = None
        self.dstFracFld = None
        self.srcGridAreas = None
        self.dstGridAreas = None
        self.pe = 0
        try:
            self.pe = MPI.COMM_WORLD.Get_rank()
        except:
            pass

        coordSys = ESMP.ESMP_COORDSYS_SPH_DEG
        periodicity = 0
        if re.search('cons', regridMethod.lower()):
            self.regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE
        if re.search('line', regridMethod.lower()):
            self.regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR
        if re.search('patch', regridMethod.lower()):
            self.regridMethod = ESMP.ESMP_REGRIDMETHOD_PATCH

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
            elif arg == 'srcMaskValues': self.srcMaskValues = args[arg]
            elif arg == 'dstMaskValues': self.dstMaskValues = args[arg]
            else:
                string = 'Unrecognized ESMP argument %s' % arg
                raise RegridError, string

        # Source Grid
        self.srcGrid = esmf.EsmfStructGrid(self.srcGridShape, 
                                coordSys = coordSys,
                                periodicity = periodicity)
        self.srcGrid.setCoords(srcGrid, staggerloc = self.staggerloc)
        if srcGridMask is not None:
            self.srcGrid.setCellMask(srcGridMask)
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
        if dstGridMask is not None:
            self.dstGrid.setCellMask(dstGridMask)
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
        sDV = numpy.array(srcGrid[0][:]) *   0.0
        dDV = numpy.array(dstGrid[0][:]) * -99.0
        self.srcDummyFld = esmf.EsmfStructField(self.srcGrid, 'srcDummyFld', 
                                           sDV, 
                                           staggerloc = self.staggerloc)
        self.dstDummyFld = esmf.EsmfStructField(self.dstGrid, 'dstDummyFld', dDV, 
                                           staggerloc = self.staggerloc)

        # Prepare the fractional area fields for conservative regridding metrics
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            self.srcFracFld = esmf.EsmfStructField(self.srcGrid, 'srcFrac',
                                       sDV,
                                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
            self.dstFracFld = esmf.EsmfStructField(self.dstGrid, 'dstFrac',
                                       dDV,
                                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
                                        

    def computeWeights(self, regridMethod = None, unMappedAction = None,
                  srcMaskValues = None, dstMaskValues = None, **args):
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
                                  srcFrac = self.srcFracFld, 
                                  dstFrac = self.dstFracFld,
                                  srcMaskValues = self.srcMaskValues,
                                  dstMaskValues = self.dstMaskValues,
                                  regridMethod = regridMethod,
                                  unMappedAction = unMappedAction)

        # Compute the weights  
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            rootPe = args.get('rootPe', 0)
            self.srcGridAreas = self.regridObj.getSrcAreas(rootPe = rootPe)
            self.dstGridAreas = self.regridObj.getDstAreas(rootPe = rootPe)
            self.srcFractions = self.regridObj.getSrcAreaFractions(rootPe = rootPe)
            self.dstFractions = self.regridObj.getDstAreaFractions(rootPe = rootPe)

    def apply(self, srcData, dstData, diagnostics = None, **args):
        """
        Regrid source to destination
        @param srcData array Full source data shape
        @param dstData array Full destination data shape
        @param rootPe set to None if dstData is locally filled. Other values
                      will gather dstData on processor rootPe
        """

        rootPe = args.get('rootPe', 0)

        self.srcDummyFld.setLocalData(srcData, self.staggerloc)

        # Regrid
        self.regridObj(self.srcDummyFld, self.dstDummyFld)

        # Get the destination data
        if rootPe is None:
            slab = self.dstGrid.getLocalSlab(staggerloc = self.staggerloc)
            dstData[slab] = self.dstDummyFld.getData(rootPe = rootPe)

        else:
            data = self.dstDummyFld.getData(rootPe = rootPe)
            if self.pe == rootPe:
                dstData[:] = data

                if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE and \
                                diagnostics is not None:
                    srcDiag = self.srcFractions * self.srcGridAreas * srcData
                    dstDiag = self.dstFractions * self.dstGridAreas * dstData
                    diagnostics.append(numpy.nansum(srcDiag) - \
                                         numpy.nansum(dstDiag))
