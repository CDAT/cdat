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
        @param coords   The curvilinear coordinates of the grid. List of numpy arrays
        @param staggerloc  The stagger location
                           ESMP.ESMP_STAGGERLOC_CENTER (default)
                           ESMP.ESMP_STAGGERLOC_CORNER
        Note: coord dims in cdms2 are ordered in y, x, but ESMF expect x, y,
        hence the dimensions are reversed here. If you are receiving unexpected
        results, try reversing the order of coordinate dimensions.
        """
        # allocate space for coordinates
        if staggerloc ==  ESMP.ESMP_STAGGERLOC_CENTER and not self.centersSet:
            ESMP.ESMP_GridAddCoord(self.grid, staggerloc=staggerloc)
            self.centersSet = True
        elif staggerloc ==  ESMP.ESMP_STAGGERLOC_CORNER and not self.nodesSet:
            ESMP.ESMP_GridAddCoord(self.grid, staggerloc=staggerloc)
            self.nodesSet = True

        for i in range(self.ndims):
            ptr = ESMP.ESMP_GridGetCoordPtr(self.grid, i+1, staggerloc)
            # Populate self.grid with coordinates or the bounds as needed
            # numpy.arrays required since numpy.ma arrays don't support flat
            ptr[:] = numpy.array(coords[self.ndims-i-1]).flat

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
        @param mask numpy array. 1 is invalid by default
        """
        ESMP.ESMP_GridAddItem(self.grid, item=ESMP.ESMP_GRIDITEM_MASK)
        maskPtr = ESMP.ESMP_GridGetItem(self.grid,
                                        item=ESMP.ESMP_GRIDITEM_MASK)
        maskPtr[:] = mask.flat

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
            ptr[:] = data.flat

    def getPointer(self, pe=None):
        """
        Get field data 
        @param pe processor rank (None if local)
        @return pointer
        """
        if pe is None:
            pe = self.pe
        return ESMP.ESMP_FieldGetPtr(self.field, localDe=pe)

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

    def getSrcAreas(self):
        """
        Get the src grid areas as used by conservative interpolation
        @return numpy array or None if interpolation is not conservative
        """
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            areaFld = EsmfStructField(self.srcField.grid, 
                                      name = 'source_grid_areas', 
                                      data = None,
                                      staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
            ESMP.ESMP_FieldRegridGetArea(areaFld.field)
            staggerloc=ESMP.ESMP_STAGGERLOC_CENTER
            shp = self.srcField.grid.getCoordShape(staggerloc=staggerloc)
            return numpy.reshape(areaFld.getPointer(), shp)
        return None

    def getDstAreas(self):
        """
        Get the dst grid areas as used by conservative interpolation
        @return numpy array or None if interpolation is not conservative
        """
        if self.regridMethod == ESMP.ESMP_REGRIDMETHOD_CONSERVE:
            areaFld = EsmfStructField(self.dstField.grid, 
                                      name = 'source_grid_areas', 
                                      data = None,
                                      staggerloc = ESMP.ESMP_STAGGERLOC_CENTER)
            ESMP.ESMP_FieldRegridGetArea(areaFld.field)
            staggerloc = ESMP.ESMP_STAGGERLOC_CENTER
            shp = self.dstField.grid.getCoordShape(staggerloc=staggerloc)
            return numpy.reshape(areaFld.getPointer(), shp)
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

def _createCLGridFromAxes(lons, lats, conserved = 0):
    """
    Contructor for a mesh using axes
    @param lons tuple/list containing start, stop, step for lons
    @param lats tuple/list containing start, stop, step for lats
    @return tuple of xyz coordinates, tuple of dimensions,
            tuple of geographic coordinates
    """
    lonb, lone, nlon = lons
    latb, late, nlat = lats

    # Geographic coordinates
    tlon = numpy.linspace(lonb, lone, nlon)   # X
    tlat = numpy.linspace(latb, late, nlat)   # Y
    lon = numpy.array(tlon, numpy.float64)
    lat = numpy.array(tlat, numpy.float64)

    dimsN = [len(lat), len(lon)]

    lon2D = getTensorProduct(lon, 1, dimsN)
    lat2D = getTensorProduct(lat, 0, dimsN)

    dimsN = lat2D.shape
    dimsE = []
    for i in dimsN: dimsE.append(i-conserved)

    # Cartesian Coordinates
    rad = numpy.pi/180.0
    XXN = numpy.cos(lat2D * rad) * numpy.cos(lon2D * rad)
    YYN = numpy.cos(lat2D * rad) * numpy.sin(lon2D * rad)
    ZZN = numpy.sin(lat2D * rad)

    return (XXN, YYN, ZZN), (dimsN, dimsE), (lon2D, lat2D)

def getTensorProduct(axis, dim, dims):
    """
    Convert an axis into a curvilinear coordinate by applying
    a tensor product. This could be replaced by numpy.meshgrid for 2D.
    @param axis 1D array of coordinates
    @param dim dimensional index of the above coordinate
    @param dims sizes of all coordinates
    @return coordinate values obtained by tensor product
    """
    return numpy.outer(numpy.outer( numpy.ones(dims[:dim], axis.dtype), axis),
                      numpy.ones(dims[dim+1:], axis.dtype)).reshape(dims)

def createPlot(srcCds, dstCds, data, vmin = 0, vmax = 0, 
               savefig = False, fileName = None):
    """
    matplotlib.pylab plots
    @param srcCds list of source coordinates lon-lat order
    @param dstCds list of destination coordinates lon-lat order
    @param data list of four data arrays source (bef, aft), dest( bef, aft)
    @param vmin minimum for plot. If not given, use data minimum
    @param vmax maximum for plot. If not given, use data maximum
    """
    import matplotlib.pylab as pl
    titles = ['Src Before', 'Src After', 'Dst Before', 'Dst After']
    Cds = [srcCds, srcCds, dstCds, dstCds]


    figure = pl.figure()
    for i in range(4):
        figure.add_subplot(2, 2, i+1)
        if vmin == 0: vmin = data[i].min()
        if vmax == 0: vmax = data[i].max()
        pl.pcolor(Cds[i][0], Cds[i][1], data[i], edgecolor = 'w',
                  vmin = vmin, vmax = vmax)
        pl.title(titles[i] + str(Cds[i][0].shape))
        pl.colorbar()
    if fileName is not None and savefig:
        pl.savefig(fileName)
    else:
        pl.show()

def writeVSH5(filename, grid, data):
    """
    Write a vizschema compliant file for an unstructured mesh
    @param filename location of output data
    @param grid esmf grid object containing xyz coordinates and cell connectivity
    @param data data
    """
    import tables 

    title = 'test'
    h5 = tables.openFile(filename, mode = 'w', title = title)
    pointname = "TestPoints"
    dataname = 'TestData'
    xgroup = h5.createGroup("/", "TestPoints", "P")
    xgroup._f_setAttr("vsType", "mesh")
    xgroup._f_setAttr("vsKind", "unstructured")
    xgroup._f_setAttr("vsQuadrilaterals", "quads")

    darray = h5.createArray("/", dataname, data.flat[:])
    darray._f_setAttr("coordinate", "lat lon")
    darray._f_setAttr("vsType", "variable")
    darray._f_setAttr("vsMesh", pointname)
    darray._f_setAttr("vsCentering", "zonal")
    h5.close()


def testCurviLinearGrid(useMethod, useStagger, writeVTK = False,
                        doPlot = False, savefig = False,
                        fileName = None):
    """
    Create a curvilinear mesh and regrid it.
    topological 2d
    spatial 3d
    @param useMethod choose between Bilinear (0, default) and Conservative (1)
    @param writeVTK Write out a vtk file for use in VisIt
    @param doPlot a X plot
    """
    method = ['bilinear', 'conservative']
    staggerlocList = [ESMP.ESMP_STAGGERLOC_CENTER, ESMP.ESMP_STAGGERLOC_CORNER]
    regridMethodList = [ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                        ESMP.ESMP_REGRIDMETHOD_CONSERVE]

    print '\nCurvilinear Coordinates --', method[useMethod]

    unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
    filepref = ['srcGrid', 'dstGrid']
    filename = {}
    for f in filepref: filename[f] = "%s_%s" % (f, method[useMethod])

    snlon, snlat = 20, 10
    dnlon, dnlat = 10,  5

    # Create the grid object
    print srcDims[0]
    srcESMFGrid = EsmfStructGrid(srcDims[0])
    srcESMFGrid.setCoords(srcCds)

    # Field Data
    srcData = numpy.ones(srcDims[useMethod], numpy.float64)

    for j in range(srcDims[useMethod][0]):
        for i in range(srcDims[useMethod][1]): srcData[j, i] = i * j

    if writeVTK:
        ESMP.ESMP_MeshWrite(srcESMFGrid.grid, filename['srcGrid'])
        writeVSH5('testSrc.vsh5', srcESMFGrid, srcData)

    srcESMFField = EsmfStructField(srcESMFGrid, 'source', srcData,
                       staggerloc = staggerlocList[useStagger])

    # Create the grid object
    maxIndex = numpy.array(dstDims[0], dtype=numpy.int32)
    dstESMFGrid = EsmfStructGrid(maxIndex)
    dstESMFGrid.setCoords(dstCds)

    # Field Data
    dstData = numpy.ones(dstDims[useMethod], numpy.float64)

    dstESMFField = EsmfStructField(dstESMFGrid, 'source', dstData,
                        staggerloc = staggerlocList[useStagger])
    if writeVTK:
        ESMP.ESMP_MeshWrite(dstESMFGrid.grid, filename['dstGrid'])
        writeVSH5('testDst.vsh5', dstESMFGrid, dstData)

    # Interpolate Bilinear
    regrid = EsmfRegrid(srcESMFField, dstESMFField,
                regridMethod = regridMethodList[useMethod],
                unMappedAction = unMappedAction)

    newSrc = numpy.reshape(srcESMFField.getPointer(), srcDims[useMethod])
    newDst = numpy.reshape(dstESMFField.getPointer(), dstDims[useMethod])

    if writeVTK:
        ESMP.ESMP_MeshWrite(dstESMFGrid.grid, filename['dstGrid'])
        writeVSH5('testDst.vsh5', dstESMFGrid, newDst)

    srcval = newSrc.sum()/newSrc.size
    dstval = newDst.sum()/newDst.size
    print '     Source Data/cell', srcval
    print 'Destination Data/cell', dstval
    print 'src/dst, src-dst   ', 100*(1-srcval/dstval), "%", srcval-dstval

    srcCds = numpy.meshgrid(numpy.linspace(0, 360, snlon-1),
                         numpy.linspace(-90, 87.712616, snlat-1))
    dstCds = numpy.meshgrid(numpy.linspace(0, 360, dnlon-1),
                         numpy.linspace(-90, 87.712616, dnlat-1))
    if doPlot:
        fileName = "%s_%s_test.png" % (regridMethodList[useMethod], 
                                       staggerlocList[useStagger])
        createPlot(srcCds, dstCds, (srcData, newSrc,
                   dstData, newDst), fileName = fileName, savefig = savefig)

    del regrid
    del dstESMFField
    del srcESMFField
    del srcESMFGrid
    del dstESMFGrid

    print ' Done'

if __name__ == "__main__":
    """
    The test allow for a vtk file to be written as well as plots to be created.
    These features are off (False) by default.
    """
    ESMP.ESMP_Initialize()
    # Curvilinear world
    testCurviLinearGrid(0, 0, writeVTK = False, doPlot = True, savefig = False)
    ESMP.ESMP_Finalize()
