#!/usr/bin/env python

"""
Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""

import re
import time
import numpy
from regrid2 import RegridError
import ESMP

# constants
R8 = ESMP.ESMP_TYPEKIND_R8
R4 = ESMP.ESMP_TYPEKIND_R4
I8 = ESMP.ESMP_TYPEKIND_I8
I4 = ESMP.ESMP_TYPEKIND_I4
CENTER = ESMP.ESMP_STAGGERLOC_CENTER # Same as ESMP_STAGGERLOC_CENTER_VCENTER
CORNER = ESMP.ESMP_STAGGERLOC_CORNER
VCORNER = ESMP.ESMP_STAGGERLOC_CORNER_VFACE
VFACE = VCORNER
CONSERVE = ESMP.ESMP_REGRIDMETHOD_CONSERVE
PATCH = ESMP.ESMP_REGRIDMETHOD_PATCH
BILINEAR = ESMP.ESMP_REGRIDMETHOD_BILINEAR
IGNORE = ESMP.ESMP_UNMAPPEDACTION_IGNORE
ERROR = ESMP.ESMP_UNMAPPEDACTION_ERROR

class EsmfUnstructGrid:
    """
    Unstructured grid
    """
    def __init__(self, numTopoDims, numSpaceDims):
        """
        Constructor
        @param numTopoDims number of topological dimensions
        @param numSpaceDims number of space dimensions
        """
        # handle to the grid object
        self.grid = None
        # whether or not nodes were added
        self.nodesAdded = False
        # whether or not cells were added
        self.cellsAdded = False
        # the local processor rank
        self.pe = 0
        # number of processors
        self.nprocs = 1
        # communicator
        self.comm = None

        vm = ESMP.ESMP_VMGetGlobal()
        self.pe, self.nprocs = ESMP.ESMP_VMGet(vm)

        self.grid = ESMP.ESMP_MeshCreate(numTopoDims, numSpaceDims)

    def setCells(self, cellIndices, cellTypes, connectivity,
                 cellMask=None, cellAreas=None):
        """
        Set Cell connectivity
        @param cell indices (0-based)
        @param cellTypes one of ESMP_MESHELEMTYPE_{TRI,QUAD,TETRA,HEX}
        @param connectivity node connectivity array, see below for node ordering
        @param cellMask
        @param cellAreas area (volume) of each cell


                     3                          4 ---------- 3
                    / \                         |            |
                   /   \                        |            |
                  /     \                       |            |
                 /       \                      |            |
                /         \                     |            |
               1 --------- 2                    1 ---------- 2



                 3                               8---------------7
                /|\                             /|              /|
               / | \                           / |             / |
              /  |  \                         /  |            /  |
             /   |   \                       /   |           /   |
            /    |    \                     5---------------6    |
           4-----|-----2                    |    |          |    |
            \    |    /                     |    4----------|----3
             \   |   /                      |   /           |   /
              \  |  /                       |  /            |  /
               \ | /                        | /             | /
                \|/                         |/              |/
                 1                          1---------------2

       ESMP_MESHELEMTYPE_TETRA             ESMP_MESHELEMTYPE_HEX

        """
        n = len(cellIndices)
        if not self.cellsAdded:
            # node/cell indices are 1-based in ESMP
            cellIndices += 1
            ESMP.ESMP_MeshAddElements(self.grid, n, cellIndices, cellTypes,
                                      connectivity, elementMask=cellMask,
                                      elementArea=cellAreas)
        self.cellsAdded = True

    def setNodes(self, indices, coords, peOwners=None):
        """
        Set the nodal coordinates
        @param indices Ids of the nodes (0-based)
        @param coords nodal coordinates
        @param peOwners processor ranks where the coordinates reside (0-based)
        """
        n = len(indices)
        if not self.nodesAdded:
            if peOwners is None:
                peOwners = numpy.ones((n,), numpy.int32) * self.pe
            # node indices are 1-based
            indices += 1
            ESMP.ESMP_MeshAddNodes(self.grid, n, indices, coords, peOwners)
        self.nodesAdded = True

    def toVTK(self, filename):
        """
        Write grid to VTK file format
        @param filename VTK file name
        """
        ESMP.ESMP_MeshWrite(self.grid, filename)


    def __del__(self):
        ESMP.ESMP_MeshDestroy(self.grid)

################################################################################

class EsmfStructGrid:
    """
    Structured grid
    """
    def __init__(self, shape, coordSys = ESMP.ESMP_COORDSYS_SPH_DEG,
                 periodicity = 0, staggerloc = ESMP.ESMP_STAGGERLOC_CENTER,
                 hasBounds = False):
        """
        Constructor
        @param shape  Tuple of cell sizes along each axis
        @param coordSys    coordinate system
                           ESMP.ESMP_COORDSYS_CART              Cartesian
                           ESMP.ESMP_COORDSYS_SPH_DEG (default) Degrees
                           ESMP.ESMP_COORDSYS_SPH_RAD           Radians
        @param periodicity Does the grid have a periodic coordinate
                           0 No periodicity
                           1 Periodic in x (1st) axis
                           2 Periodic in x, y axes
        @param staggerloc ESMP stagger location. ESMP.ESMP_STAGGERLOC_XXXX
                          The stagger constants are listed at the top
        @param hasBounds If the grid has bounds, Run AddCoords for the bounds
        """
        # ESMF grid object
        self.grid = None
        # number of cells in [z,] y, x on all processors
        self.shape = shape
        # number of dimensions
        self.ndims = len(self.shape)
        # whether or not cell areas were set
        self.cellAreasSet = False
        # whether or not nodal coords were set
        self.nodesSet = False
        # whether or not cell centered coordinates were set
        self.centersSet = False

        # ESMF index order is opposite to C order, we have order
        # y, x whereas ESMF assumes x, y
        maxIndex = numpy.array(shape[::-1], dtype = numpy.int32)

        if periodicity == 0:
            self.grid = ESMP.ESMP_GridCreateNoPeriDim(maxIndex,
                                                      coordSys = coordSys)
        elif periodicity == 1:
            self.grid = ESMP.ESMP_GridCreate1PeriDim(maxIndex,
                                                     coordSys = coordSys)
        else:
            msg = """
esmf.EsmfStructGrid.__init__: ERROR periodic dimensions %d > 1 not permitted.
            """ % periodicity
            raise RegridError, msg

        # Grid add coordinates call must go here for parallel runs
        # This occur before the fields are created, making the fields
        # parallel aware.
        ESMP.ESMP_GridAddCoord(self.grid, staggerloc=staggerloc)
        if staggerloc ==  CENTER and not self.centersSet:
            self.centersSet = True
        elif staggerloc ==  CORNER and not self.nodesSet:
            self.nodesSet = True

        if hasBounds is not None:
            if self.ndims == 2:
                ESMP.ESMP_GridAddCoord(self.grid, staggerloc = CORNER)
            if self.ndims == 3:
                ESMP.ESMP_GridAddCoord(self.grid, staggerloc = VCORNER)

    def getLocalSlab(self, staggerloc):
        """
        Get the local slab (ellipsis). You can use this to grab
        the data local to this processor
        @param staggerloc (e.g. ESMP.ESMP_STAGGERLOC_CENTER)
        @return tuple of slices
        """
        lo, hi = self.getLoHiBounds(staggerloc)
        return tuple([slice(lo[i], hi[i], None) \
                          for i in range(self.ndims)])

    def getLoHiBounds(self, staggerloc):
        """
        Get the local lo/hi index values for the coordinates (per processor)
                         (hi is not inclusive, lo <= index < hi)
        @param staggerloc e.g. ESMP.ESMP_STAGGERLOC_CENTER
        @return lo, hi lists
        """
        lo, hi = ESMP.ESMP_GridGetCoord(self.grid, staggerloc)
        # reverse order since ESMF follows fortran order
        return lo[::-1], hi[::-1]

    def getCoordShape(self, staggerloc):
        """
        Get the local coordinate shape (may be different on each processor)
        @param staggerloc (e.g. ESMP.ESMP_STAGGERLOC_CENTER)
        @return tuple
        """
        lo, hi = self.getLoHiBounds(staggerloc)
        return tuple( [hi[i] - lo[i] for i in range(self.ndims)] )

    def setCoords(self, coords, staggerloc = CENTER, globalIndexing = False):
        """
        Populate the grid with staggered coordinates (e.g. corner or center).
        @param coords   The curvilinear coordinates of the grid.
                        List of numpy arrays. Must exist on all procs.
        @param staggerloc  The stagger location
                           ESMP.ESMP_STAGGERLOC_CENTER (default)
                           ESMP.ESMP_STAGGERLOC_CORNER
        @param globalIndexing if True array was allocated over global index
                              space, otherwise array was allocated over
                              local index space on this processor. This
                              is only relevant if rootPe is None
        Note: coord dims in cdms2 are ordered in y, x, but ESMF expects x, y,
        hence the dimensions are reversed here.
        """
        # allocate space for coordinates, can only add coordinates once

        for i in range(self.ndims):
            ptr = ESMP.ESMP_GridGetCoordPtr(self.grid, i, staggerloc)
            if globalIndexing:
                slab = self.getLocalSlab(staggerloc)
                # Populate self.grid with coordinates or the bounds as needed
                ptr[:] = coords[self.ndims-i-1][slab].flat
            else:
                ptr[:] = coords[self.ndims-i-1].flat

    def getCoords(self, dim, staggerloc):
        """
        Return the coordinates for a dimension
        @param dim desired dimension (zero based indexing)
        @param staggerloc Stagger location
        """
        gridPtr = ESMP.ESMP_GridGetCoordPtr(self.grid, dim, staggerloc)
        shp = self.getCoordShape(staggerloc)
        return numpy.reshape(gridPtr, shp)

    def setCellAreas(self, areas):
        """
        Set the cell areas
        @param areas numpy array
        """
        ESMP.ESMP_GridAddItem(self.grid, item=ESMP.ESMP_GRIDITEM_AREA)
        areaPtr = ESMP.ESMP_GridGetItem(self.grid,
                                        item=ESMP.ESMP_GRIDITEM_AREA)
        areaPtr[:] = areas.flat
        self.cellAreasSet = True

    def getCellAreas(self):
        """
        @return cell areas or None if setCellAreas was not called
        """
        if self.cellAreasSet:
            areaPtr = ESMP.ESMP_GridGetItem(self.grid,
                                            item = ESMP.ESMP_GRIDITEM_AREA)
            return numpy.reshape(areaPtr, self.shape)
        else:
            return None

    def setMask(self, mask):
        """
        Set mask array. In ESMF, the mask is applied to cells.
        @param mask numpy array. 1 is invalid by default. This array exists
                    on all procs
        """
        ESMP.ESMP_GridAddItem(self.grid, item=ESMP.ESMP_GRIDITEM_MASK)
        maskPtr = ESMP.ESMP_GridGetItem(self.grid,
                                        item=ESMP.ESMP_GRIDITEM_MASK)
        slab = self.getLocalSlab(CENTER)
        maskPtr[:] = mask[slab].flat

    def __del__(self):
        ESMP.ESMP_GridDestroy(self.grid)

################################################################################

class EsmfStructField:
    """
    Structured field.
    """
    def __init__(self, esmfGrid, name, datatype, staggerloc = CENTER):
        """
        Creator for structured ESMF Field
        @param esmfGrid instance of an ESMP_Grid
        @param name field name (must be unique)
        @param datatype data type, one of 'float64', 'float32', 'int64', or 'int32'
                        (or equivalent numpy dtype)
        @param staggerloc ESMP_STAGGERLOC_CENTER
                          ESMP_STAGGERLOC_CORNER
        """
        # field object
        self.field = None
        # the local processor rank
        self.pe = 0
        # the number of processors
        self.nprocs = 1
        # associated grid
        self.grid = esmfGrid
        # staggering
        self.staggerloc = staggerloc
        # communicator
        self.comm = None

        try:
            from mpi4py import MPI
            self.comm = MPI.COMM_WORLD
        except:
            pass

        vm = ESMP.ESMP_VMGetGlobal()
        self.pe, self.nprocs = ESMP.ESMP_VMGet(vm)

        etype = None
        sdatatype = str(datatype) # in case user passes a numpy dtype
        if re.search('float64', sdatatype):
            etype = R8
        elif re.search('float32', sdatatype):
            etype = R4
        elif re.search('int64', sdatatype):
            etype = I8
        elif re.search('int32', sdatatype):
            etype = I4
        else:
            msg = 'esmf.EsmfStructField.__init__: ERROR invalid type %s' % datatype
            raise RegridError, msg

        self.field = ESMP.ESMP_FieldCreateGrid(esmfGrid.grid,
                                               name,
                                               staggerloc = staggerloc,
                                               typekind = etype)

    def getPointer(self):
        """
        Get field data as a flat array
        @return pointer
        """
        return ESMP.ESMP_FieldGetPtr(self.field)

    def getData(self, rootPe):
        """
        Get field data as a numpy array
        @param rootPe if None then local data will be fetched, otherwise
                      gather the data on processor "rootPe" (all other
                      procs will return None).
        @return numpy array or None
        """
        ptr = self.getPointer()
        if rootPe is None:
            shp = self.grid.getCoordShape(staggerloc = self.staggerloc)
            # local data, copy
            return ptr.reshape(shp)
        else:
            # gather the data on rootPe
            lo, hi = self.grid.getLoHiBounds(self.staggerloc)
            los = [lo]
            his = [hi]
            ptrs = [ptr]
            if self.comm is not None:
                los = self.comm.allgather(lo)
                his = self.comm.allgather(hi)
                ptrs = self.comm.gather(ptr, root = rootPe)

                # reassemble, find the largest hi indices to set
                # the shape of the data container
                bigHi = [0 for i in range(self.grid.ndims)]
                for i in range(self.grid.ndims):
                    bigHi[i] = reduce(lambda x,y: max(x, y),
                                      [his[p][i] for p in range(self.nprocs)])
                # allocate space to retrieve the data
                bigData = numpy.empty(bigHi, ptr.dtype)
                bigData[:] = 0.0

            # populate the data
            if self.pe == rootPe:
                for p in range(self.nprocs):
                    slab = tuple([slice(los[p][i], his[p][i], None) for \
                                      i in range(self.grid.ndims)])
                    # copy
                    bigData[slab].flat = ptrs[p]

            if self.comm is not None:
                self.comm.barrier()
                bigData = self.comm.bcast(obj = bigData, root = rootPe)
            return bigData

        # rootPe is not None and self.pe != rootPe
        return None

    def setLocalData(self, data, staggerloc, globalIndexing = False):
        """
        Set local field data
        @param data full numpy array, this method will take care of setting a
                    the subset of the data that reside on the local processor
        @param staggerloc stagger location of the data
        @param globalIndexing if True array was allocated over global index
                              space, array was allocated over local index
                              space (on this processor)
        """
        ptr = self.getPointer()
        if globalIndexing:
            slab = self.grid.getLocalSlab(staggerloc)
            ptr[:] = data[slab].flat
        else:
            ptr[:] = data.flat

    def  __del__(self):
        ESMP.ESMP_FieldDestroy(self.field)

################################################################################

class EsmfRegrid:
    """
    Regrid source grid data to destination grid data
    """
    def __init__(self, srcField, dstField,
                 srcFrac = None,
                 dstFrac = None,
                 srcMaskValues = None,
                 dstMaskValues = None,
                 regridMethod   = BILINEAR,
                 unMappedAction = IGNORE):
        """
        Constuct regrid object
        @param srcField the source field object of type EsmfStructField
        @param dstField the destination field object of type EsmfStructField
        @param srcMaskValues Value of masked cells in source
        @param dstMaskValues Value of masked cells in destination
        @param srcFrac Cell fractions on source grid (type EsmfStructField)
        @param dstFrac Cell fractions on destination grid (type EsmfStructField)
        @param regridMethod ESMP.ESMP_REGRIDMETHOD_{BILINEAR,CONSERVE,PATCH}
        @param unMappedAction ESMP.ESMP_UNMAPPEDACTION_{IGNORE,ERROR}
        """
        self.srcField = srcField
        self.dstField = dstField
        self.regridMethod = regridMethod
        self.srcAreaField = None
        self.dstAreaField = None
        self.srcFracField = srcFrac
        self.dstFracField = dstFrac
        self.regridHandle = None

        timeStamp = re.sub('\.', '', str(time.time()))

        # create and initialize the cell areas to zero
        if regridMethod == CONSERVE:
            self.srcAreaField = EsmfStructField(self.srcField.grid,
                                                name = 'src_areas_%s' % timeStamp,
                                                datatype = 'float64',
                                                staggerloc = CENTER)
            dataPtr = self.srcAreaField.getPointer()
            dataPtr[:] = 0.0
            self.dstAreaField = EsmfStructField(self.dstField.grid,
                                                name = 'dst_areas_%s' % timeStamp,
                                                datatype = 'float64',
                                                staggerloc = CENTER)
            dataPtr = self.dstAreaField.getPointer()
            dataPtr[:] = 0.0

        # initialize fractional areas to 1 (unless supplied)
        if srcFrac is None:
            self.srcFracField = EsmfStructField(self.srcField.grid,
                                                name = 'src_cell_area_fractions_%s' % timeStamp,
                                                datatype = 'float64',
                                                staggerloc = CENTER)
            dataPtr = self.srcFracField.getPointer()
            dataPtr[:] = 1.0

        if dstFrac is None:
            self.dstFracField = EsmfStructField(self.dstField.grid,
                                                name = 'dst_cell_area_fractions_%s' % timeStamp,
                                                datatype = 'float64',
                                                staggerloc = CENTER)
            dataPtr = self.dstFracField.getPointer()
            dataPtr[:] = 1.0

        srcMaskValueArr = None
        if srcMaskValues is not None:
            srcMaskValueArr = numpy.array(srcMaskValues, dtype=numpy.int32)

        dstMaskValueArr = None
        if dstMaskValues is not None:
            dstMaskValueArr = numpy.array(dstMaskValues, dtype=numpy.int32)

        self.regridHandle = ESMP.ESMP_FieldRegridStore(
                                     srcField.field,
                                     dstField.field,
                                     srcMaskValues = srcMaskValueArr,
                                     dstMaskValues = dstMaskValueArr,
                                     srcFracField = self.srcFracField.field,
                                     dstFracField = self.dstFracField.field,
                                     regridmethod = regridMethod,
                                     unmappedaction = unMappedAction)

    def getSrcAreas(self, rootPe):
        """
        Get the src grid areas as used by conservative interpolation
        @param rootPe None is local areas are returned, otherwise
                      provide rootPe and the data will be gathered
        @return numpy array or None if interpolation is not conservative
        """
        if self.srcAreaField is not None:
            ESMP.ESMP_FieldRegridGetArea(self.srcAreaField.field)
            return self.srcAreaField.getData(rootPe = rootPe)
        return None

    def getDstAreas(self, rootPe):
        """
        Get the dst grid areas as used by conservative interpolation
        @param rootPe None is local areas are returned, otherwise
                      provide rootPe and the data will be gathered
        @return numpy array or None if interpolation is not conservative
        """
        if self.srcAreaField is not None:
            ESMP.ESMP_FieldRegridGetArea(self.dstAreaField.field)
            return self.dstAreaField.getData(rootPe = rootPe)
        return None

    def getSrcAreaFractions(self, rootPe):
        """
        Get the source grid fraction areas as used by conservative interpolation
        @param rootPe None is local areas are returned, otherwise
                      provide rootPe and the data will be gathered
        @return numpy array
        """
        if self.srcFracField is not None:
            return self.srcFracField.getData(rootPe = rootPe)
        return None

    def getDstAreaFractions(self, rootPe):
        """
        Get the destination grid fraction areas as used by conservative interpolation
        @param rootPe None is local areas are returned, otherwise
                      provide rootPe and the data will be gathered
        @return numpy array
        """
        if self.dstFracField is not None:
            return self.dstFracField.getData(rootPe = rootPe)
        return None

    def __call__(self, srcField=None, dstField=None):
        """
        Apply interpolation weights
        @param srcField source field (or None if src field passed to
               constructor is to be used)
        @param dstField destination field (or None if dst field passed
               to constructor is to be used)
        """
        if srcField == None:
            srcField = self.srcField
        if dstField == None:
            dstField = self.dstField

        # default is keep the masked values intact
        zeroregion = ESMP.ESMP_REGION_SELECT
        if self.regridMethod == CONSERVE:
            zeroregion = None # will initalize to zero

        ESMP.ESMP_FieldRegrid(srcField.field, dstField.field,
                              self.regridHandle,
                              zeroregion = zeroregion)

    def __del__(self):
        if self.regridHandle is not None:
            ESMP.ESMP_FieldRegridRelease(self.regridHandle)

