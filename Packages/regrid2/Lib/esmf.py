#!/usr/bin/env python

"""
Copyright (c) 2008-2012, Tech-X Corporation
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the conditions
specified in the license file 'license.txt' are met.

Authors: David Kindig and Alex Pletzer
"""

import numpy
import cdms2
from regrid2 import RegridError
import ESMP

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
        # number of porcessors
        self.nprocs = 1

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
            #connectivity += 1 # connectivity is zero-based!
            print 'cellIndices = ', cellIndices
            print 'connectivity = ', connectivity
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
                 periodicity = 0):
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

        """
        # ESMF grid object
        self.grid = None
        # number of cells in [z,] y, x
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
        elif periodicity == 2:
            self.grid = ESMP.ESMP_GridCreate2PeriDim(maxIndex,
                                                     coordSys = coordSys)
        else:
            raise RegridError, "Periodic dimensions > 2 not permitted."

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
        Get the lo/hi index values for the coordinates 
                         (hi is not inclusive, lo <= index < hi)
        @param staggerloc (e.g. ESMP.ESMP_STAGGERLOC_CENTER)
        @return lo, hi lists
        """
        lo, hi = ESMP.ESMP_GridGetCoord(self.grid, staggerloc)
        # reverse order since ESMF follows fortran order
        return lo[::-1], hi[::-1]

    def getCoordShape(self, staggerloc):
        """
        Get the coordinate shape 
        @param staggerloc (e.g. ESMP.ESMP_STAGGERLOC_CENTER)
        @return tuple 
        """
        lo, hi = self.getLoHiBounds(staggerloc)
        return tuple( [hi[i] - lo[i] for i in range(self.ndims)] )

    def setCoords(self, coords, staggerloc = ESMP.ESMP_STAGGERLOC_CENTER):
        """
        Populate the grid with staggered coordinates (e.g. corner or center). 
        @param coords   The curvilinear coordinates of the grid. 
                        List of numpy arrays. Must exist on all procs.
        @param staggerloc  The stagger location
                           ESMP.ESMP_STAGGERLOC_CENTER (default)
                           ESMP.ESMP_STAGGERLOC_CORNER
        Note: coord dims in cdms2 are ordered in y, x, but ESMF expects x, y,
        hence the dimensions are reversed here.
        """
        # allocate space for coordinates, can only add coordinates once
        if staggerloc ==  ESMP.ESMP_STAGGERLOC_CENTER and not self.centersSet:
            ESMP.ESMP_GridAddCoord(self.grid, staggerloc=staggerloc)
            self.centersSet = True
        elif staggerloc ==  ESMP.ESMP_STAGGERLOC_CORNER and not self.nodesSet:
            ESMP.ESMP_GridAddCoord(self.grid, staggerloc=staggerloc)
            self.nodesSet = True

        for i in range(self.ndims):
            ptr = ESMP.ESMP_GridGetCoordPtr(self.grid, i+1, staggerloc)
            slab = self.getLocalSlab(staggerloc)
            # Populate self.grid with coordinates or the bounds as needed
            # numpy.arrays required since numpy.ma arrays don't support flat
            ptr[:] = numpy.array(coords[self.ndims-i-1][slab]).flat

    def getCoords(self, dim, staggerloc):
        """
        Return the coordinates for a dimension
        @param dim desired dimension (zero based indexing)
        @param staggerloc Stagger location
        """
        # esmf uses 1-based indexing
        gridPtr = ESMP.ESMP_GridGetCoordPtr(self.grid, dim+1, staggerloc)
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

    def setCellMask(self, mask):
        """
        Set cell mask
        @param mask numpy array. 1 is invalid by default. This array exists
                    on all procs
        """
        ESMP.ESMP_GridAddItem(self.grid, item=ESMP.ESMP_GRIDITEM_MASK)
        maskPtr = ESMP.ESMP_GridGetItem(self.grid,
                                        item=ESMP.ESMP_GRIDITEM_MASK)
        slab = self.getLocalSlab(ESMP.ESMP_STAGGERLOC_CENTER)
        maskPtr[:] = mask[slab].flat

    def getCellMask(self):
        """
        @return a mask pointer
        """
        maskPtr = ESMP.ESMP_GridGetItem(self.grid,
                                        item = ESMP.ESMP_GRIDITEM_MASK)
        return numpy.reshape(maskPtr, self.shape)

    def __del__(self):
        ESMP.ESMP_GridDestroy(self.grid)

################################################################################

class EsmfStructField:
    """
    Structured field.
    """
    def __init__(self, esmfGrid, name, data = None,
                 staggerloc = ESMP.ESMP_STAGGERLOC_CENTER):
        """
        Creator for ESMF Field
        @param esmfGrid instance of an ESMP_Grid
        @param name field name (must be unique)
        @param data numpy ndarray of data
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

        if data is not None:
            numpyType2EsmfType = {
                'float64': ESMP.ESMP_TYPEKIND_R8,
                'float32': ESMP.ESMP_TYPEKIND_R4,
                'int64': ESMP.ESMP_TYPEKIND_I8,
                'int32': ESMP.ESMP_TYPEKIND_I4, }
            etype = numpyType2EsmfType[str(data.dtype)]
        else:
            etype = ESMP.ESMP_TYPEKIND_R8

        self.field = ESMP.ESMP_FieldCreateGrid(esmfGrid.grid, name,
                        staggerloc = staggerloc,
                        typekind = etype)

        # Copy the data
        if data is not None:
            ptr = self.getPointer()
            slab = self.grid.getLocalSlab(self.staggerloc)
            ptr[:] = data[slab].flat

    def getPointer(self):
        """
        Get field data as a flat array
        @return pointer
        """
        return ESMP.ESMP_FieldGetPtr(self.field)

    def getData(self, rootPe = None):
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
            # local data
            return ptr.reshape(shp)
        else:
            # gather the data on rootPe
            lo, hi = self.grid.getLoHiBounds(self.staggerloc)
            los = self.comm.gather(lo, root = rootPe)
            his = self.comm.gather(hi, root = rootPe)
            ptrs = self.comm.gather(ptr, root = rootPe)
            if self.pe == rootPe:
                # reassemble, find the larges hi indices
                bigHi = [0 for i in range(self.grid.ndims)]
                for i in range(self.grid.ndims):
                    bigHi[i] = reduce(lambda x,y: max(x, y), 
                                      [his[i][p] for p in range(self.nprocs)])
                # allocate space to retieve the data
                bigData = numpy.empty(bigHi, ptr.dtype)
                for p in range(self.nprocs):
                    slab = tuple([slice(los[p][i], his[p][i], None) for \
                                      i in range(self.grid.ndims)])
                    # copy
                    bigData[slab].flat = ptr
                return bigData
        # rootPe is not None and self.pe != rootPe
        return None
                                                               

    def  __del__(self):
        ESMP.ESMP_FieldDestroy(self.field)

################################################################################

class EsmfRegrid:
    """
    Regrid source grid data to destination grid data
    """
    def __init__(self, srcField, dstField,
                 srcFrac = None, dstFrac = None,
                 srcMaskValues = None,
                 dstMaskValues = None,
                 regridMethod   = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                 unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE):
        """
        Regrid
        @param srcField the source field object of type EsmfStructField
        @param dstField the destination field object of type EsmfStructField

        Optional:
        @param srcMaskValues Value of masked cells in source
        @param dstMaskValues Value of masked cells in destination
                     e.g.
        @param regridMethod ESMF constanct bilinear, conservative, etc.
        @param unMappedAction ESMP Constant Error, Ignore, etc.
        @param srcFrac Source fraction of cells used - Output value
        @param dstFrac Source fraction of cells used - Output value
        """
        self.srcField = srcField
        self.dstField = dstField
        self.regridMethod = regridMethod

        srcMaskValueArr = None
        if srcMaskValues is not None:
            srcMaskValueArr = numpy.array(srcMaskValues, dtype=numpy.int32)

        dstMaskValueArr = None
        if dstMaskValues is not None:
            dstMaskValueArr = numpy.array(dstMaskValues, dtype=numpy.int32)

        self.srcFracField = None
        self.dstFracField = None
        if srcFrac is not None: self.srcFracField = srcFrac.field
        if dstFrac is not None: self.dstFracField = dstFrac.field

        self.regridHandle = ESMP.ESMP_FieldRegridStore(
                                     srcField.field,
                                     dstField.field,
                                     srcMaskValues = srcMaskValueArr,
                                     dstMaskValues = dstMaskValueArr,
                                     srcFracField = self.srcFracField,
                                     dstFracField = self.dstFracField,
                                     regridmethod = regridMethod,
                                     unmappedaction = unMappedAction)

    def getSrcAreas(self, rootPe = None):
        """
        Get the src grid areas as used by conservative interpolation
        @param rootPe None is local areas are returned, otherwise
                      provide rootPe and the data will be gathered
        @return numpy array or None if interpolation is not conservative
        """
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            areaFld = EsmfStructField(self.srcField.grid, 
                                      name = 'source_grid_areas', 
                                      data = None,
                                      staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
            ESMP.ESMP_FieldRegridGetArea(areaFld.field)
            staggerloc=ESMP.ESMP_STAGGERLOC_CENTER
            return areaFld.getData(rootPe)
        return None

    def getDstAreas(self, rootPe = None):
        """
        Get the dst grid areas as used by conservative interpolation
        @param rootPe None is local areas are returned, otherwise
                      provide rootPe and the data will be gathered        
        @return numpy array or None if interpolation is not conservative
        """
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            areaFld = EsmfStructField(self.dstField.grid, 
                                      name = 'source_grid_areas', 
                                      data = None,
                                      staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
            ESMP.ESMP_FieldRegridGetArea(areaFld.field)
            return areaFld.getData(rootPe)
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
        ESMP.ESMP_FieldRegrid(srcField.field, dstField.field, self.regridHandle)

    def __del__(self):
        ESMP.ESMP_FieldRegridRelease(self.regridHandle)

