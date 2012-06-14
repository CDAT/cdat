"""
ESMF regridding class

Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""
import types
import re
import numpy

import ESMP
from regrid2 import esmf
from regrid2 import RegridError
from regrid2 import GenericRegrid
from regrid2 import RegridError

HAVE_MPI = False
try:
    from mpi4py import MPI
    HAVE_MPI = True
except:
    pass

class ESMFRegrid(GenericRegrid):
    """
    Regrid class for ESMF
    """
    def __init__(self, srcGrid, dstGrid, 
                 regridMethod, staggerLoc, periodicity, coordSys,
                 srcGridMask = None, srcBounds = None, srcGridAreas = None,
                 dstGridMask = None, dstBounds = None, dstGridAreas = None):
        """
        Constructor
        @param srcGrid list [[z], y, x] of source grid arrays
        @param dstGrid list [[z], y, x] of dstination grid arrays
        @param regridMethod 'linear', 'conserve', or 'patch'
        @param staggerLoc the staggering of the field, 'center' or 'corner'
        @param periodicity 0 (no periodicity), 
                           1 (last coordinate is periodic, 
                           2 (both coordinates are periodic)
        @param coordSys 'deg', 'cart', or 'rad'
        @param srcGridMask list [[z], y, x] of arrays
        @param srcBounds list [[z], y, x] of arrays
        @param srcGridAreas list [[z], y, x] of arrays
        @param dstGridMask list [[z], y, x] of arrays
        @param dstBounds list [[z], y, x] of arrays
        @param dstGridAreas list [[z], y, x] of arrays
        """

        # esmf grid objects (tobe constructed)
        self.srcGrid = None
        self.dstGrid = None

        srcGridShape = srcGrid[0].shape
        dstGridShape = dstGrid[0].shape
        self.ndims = len(srcGridShape)

        self.regridMethod = ESMP.ESMP_REGRIDMETHOD_BILINEAR
        if type(regridMethod) == types.StringType:
            if re.search('conserv', regridMethod):
                self.regridMethod = ESMP.ESMP_REGRIDMETHOD_CONSERVE
            elif re.search('patch', regridMethod):
                self.regridMethod = ESMP.ESMP_REGRIDMETHOD_PATCH

        # data stagger
        self.staggerloc = ESMP.ESMP_STAGGERLOC_CENTER
        if type(staggerLoc) == types.StringType:
            if re.search('corner', staggerLoc, re.I) or \
                    re.search('node', staggerLoc, re.I):
                self.staggerloc = ESMP.ESMP_STAGGERLOC_CORNER
        
        # good for now
        self.unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE

        self.coordSys = ESMP.ESMP_COORDSYS_SPH_DEG
        if re.search('cart', coordSys):
            self.coordSys = ESMP.ESMP_COORDSYS_CART
        elif re.search('rad', coordSys):
            self.coordSys = ESMP.ESMP_COORDSYS_SPH_RAD

        self.periodicity = periodicity

        # masks can take several values in ESMF
        self.srcMaskValues = numpy.array([1],dtype = numpy.int32)
        self.dstMaskValues = numpy.array([1],dtype = numpy.int32)

        # provided by user or None
        self.srcGridAreas = srcGridAreas
        self.dstGridAreas = dstGridAreas

        # fractional area fields, to be filled in
        self.srcFracFld = None
        self.dstFracFld = None

        # MPI stuff
        self.pe = 0
        self.nprocs = 1
        self.comm = None
        if HAVE_MPI:
            self.comm = MPI.COMM_WORLD
            self.pe = self.comm.Get_rank()
            self.nprocs = self.comm.Get_size()

        # checks
        if self.ndims != len(dstGridShape):
            msg = """
mvESMFRegrid.ESMFRegrid.__init__: mismatch in the number of topological 
dimensions. len(srcGrid[0].shape) = %d != len(dstGrid[0].shape)""" % \
                (self.ndims, len(dstGridShape))
            raise RegridError, msg

        # create esmf source Grid
        self.srcGrid = esmf.EsmfStructGrid(srcGridShape, 
                                coordSys = self.coordSys,
                                periodicity = self.periodicity)
        self.srcGrid.setCoords(srcGrid, staggerloc = self.staggerloc)

        # we only need one value the mask elements can take
        if srcGridMask is not None:
            self.srcGrid.setCellMask(srcGridMask)

        if srcBounds is not None:
        # Coords are CENTER (cell) based, bounds are CORNER (nodal)
            if self.staggerloc != ESMP.ESMP_STAGGERLOC_CORNER:
                self.srcGrid.setCoords(srcBounds, 
                               staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
            elif self.staggerloc == ESMP.ESMP_STAGGERLOC_CORNER:
                msg = """
mvESMFRegrid.ESMFRegrid.__init__: can't set the src bounds for 
staggerLoc = %s!""" % staggerLoc
                raise RegridError, msg

        # create destination Grid
        self.dstGrid = esmf.EsmfStructGrid(dstGrid[0].shape, 
                                coordSys = self.coordSys,
                                periodicity = self.periodicity)
        self.dstGrid.setCoords(dstGrid, staggerloc = self.staggerloc)
        if dstGridMask is not None:
            self.dstGrid.setCellMask(dstGridMask)

        if dstBounds is not None:
            # Coords are CENTER (cell) based, bounds are CORNER (nodal)
            if self.staggerloc != ESMP.ESMP_STAGGERLOC_CORNER:
                self.dstGrid.setCoords(dstBounds, 
                                   staggerloc = ESMP.ESMP_STAGGERLOC_CORNER)
            elif self.staggerloc == ESMP.ESMP_STAGGERLOC_CORNER:
                msg = """
mvESMFRegrid.ESMFRegrid.__init__: can't set the dst bounds for 
staggerLoc = %s!""" % staggerLoc
                raise RegridError, msg

        # dummy fields required in order to instantiate the regrid object
        sDV = numpy.array(srcGrid[0][:]) * 0.0
        dDV = numpy.array(dstGrid[0][:]) * 0.0
        self.srcFld = esmf.EsmfStructField(self.srcGrid, 'srcFld', 
                                                sDV, 
                                                staggerloc = self.staggerloc)
        self.dstFld = esmf.EsmfStructField(self.dstGrid, 'dstFld', 
                                                dDV, 
                                                staggerloc = self.staggerloc)

        # prepare the fractional area fields for conservativation check
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            self.srcFracFld = esmf.EsmfStructField(self.srcGrid, 'srcFrac',
                                       sDV,
                                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
            self.dstFracFld = esmf.EsmfStructField(self.dstGrid, 'dstFrac',
                                       dDV,
                                       staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
                                        

    def computeWeights(self, **args):
        """
        Compute Weights
        @param **args (not used)
        """
        self.regridObj = esmf.EsmfRegrid(self.srcFld, self.dstFld,
                                  srcFrac = self.srcFracFld, 
                                  dstFrac = self.dstFracFld,
                                  srcMaskValues = self.srcMaskValues,
                                  dstMaskValues = self.dstMaskValues,
                                  regridMethod = self.regridMethod,
                                  unMappedAction = self.unMappedAction)

    def apply(self, srcData, dstData, **args):
        """
        Regrid source to destination
        @param srcData array Full source data shape
        @param dstData array Full destination data shape
        @param **args  expect rootPe keyword argument
                       rootPe: processor id where data should be gathered
                               or None if local data are to be returned
        """

        rootPe = args.get('rootPe', 0)

        self.srcFld.setLocalData(srcData, self.staggerloc)

        # Regrid
        self.regridObj(self.srcFld, self.dstFld)

        # Get the destination data
        if rootPe is None:
            slab = self.dstGrid.getLocalSlab(staggerloc = self.staggerloc)
            dstData[slab] = self.dstFld.getData(rootPe = rootPe)

        else:
            data = self.dstFld.getData(rootPe = rootPe)
            if self.pe == rootPe:
                dstData[:] = data

    def getDstGrid(self):
        """
        Get the destination grid on this processor
        @return grid
        """
        return [self.dstGrid.getCoords(i, staggerloc=self.staggerloc) \
                    for i in range(self.ndims)]

    def getSrcAreas(self, rootPe = None):
        """
        Get the source grid cell areas
        @param rootPe root processor where data should be gathered (or 
                      None if local areas are to be returned)
        @return areas or None if non-conservative interpolation
        """
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            return self.regridObj.getSrcAreas(rootPe = rootPe)
        else:
            return None
        

    def getDstAreas(self, rootPe = None):
        """
        Get the destination grid cell areas
        @param rootPe root processor where data should be gathered (or 
                      None if local areas are to be returned)
        @return areas or None if non-conservative interpolation
        """
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            return self.regridObj.getDstAreas(rootPe = rootPe)
        else:
            return None

    def getSrcAreaFractions(self, rootPe = None):
        """
        Get the source grid area fractions
        @param rootPe root processor where data should be gathered (or 
                      None if local areas are to be returned)
        @return fractional areas or None (if non-conservative)
        """
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            return self.regridObj.getSrcAreaFractions(rootPe = rootPe)
        else:
            return None

    def getDstAreaFractions(self, rootPe = None):
        """
        Get the destination grid area fractions
        @param rootPe root processor where data should be gathered (or 
                      None if local areas are to be returned)
        @return fractional areas or None (if non-conservative)
        """
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            return self.regridObj.getSrcAreaFractions(rootPe = rootPe)
        else:
            return None
