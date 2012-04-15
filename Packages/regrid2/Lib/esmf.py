import ESMP
import numpy as np
import operator
import cdms2
import tables
import sys

# Global variables
method = ['bilinear', 'conservative']
meshloc = [ESMP.ESMP_MESHLOC_NODE, ESMP.ESMP_MESHLOC_ELEMENT]
regridMethod = [ESMP.ESMP_REGRIDMETHOD_BILINEAR, ESMP.ESMP_REGRIDMETHOD_CONSERVE]

def initialize():
    """
    Initialize ESMP
    """
    ESMP.ESMP_Initialize()

def finalize():
    """Finalize (close) ESMP"""
    ESMP.ESMP_Finalize()
    
class EsmfGrid:
    def __init__(self, upperBound, lowerBound, coordsCor = None, 
                 coordsCtr = None, mask = None, periodicity = 1,
                 coordSys = ESMP.ESMP_COORDSYS_CART):
        """
        Construct an ESMP Grid object
        @param upperBoundXY tuple Upper Dimension of the grid in x, y (lon, lat)
        @param lowerBoundXY tuple Lower dimension of the grid in x, y
        @param coordsCor tuple Corner coordinates of the grid (lon, lat)
        @param coordsCtr tuple Center coordinates of the grid (lon, lat)
        @param mask Mask 1 is invalid data 0 valid (numpy definition)
        @param periodicity Number of periodic boundaries
                           0 - None
                           1 - One e.g. longitude default (Assume global)
                           2 - Two e.g.
        @param coordSys (default) ESMP_COORDSYS_CART
                                  ESMP_COORDSYS_SPH_DEG
                                  ESMP_COORDSYS_SPH_RAD
        """
        coordSystems = [ESMP.ESMP_COORDSYS_CART, ESMP.ESMP_COORDSYS_SPH_DEG,
                        ESMP.ESMP_COORDSYS_SPH_RAD]
        if coordSys not in coordSystems:
            raise cdms2.CDMSError, """
                  Coordinate system must be ESMP.ESMP_COORDSYS_CART
                                            ESMP.ESMP_COORDSYS_SPH_DEG
                                            ESMP.ESMP_COORDSYS_SPH_RAD"""
        rank = len(upperBound)
        maxList = []
        for i in range(rank):
            maxList.append(upperBound[i] - lowerBound[i])
        maxIndex = np.array(maxList, dtype = np.int32)
        
        # Create the grid object
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
            raise cdms2.CDMSError, """
                    Periodicity must be 0, 1 or 2"""

        locs = [ESMP.ESMP_STAGGERLOC_CORNER, ESMP.ESMP_STAGGERLOC_CENTER]
        cLoc = [coordsCor, coordsCtr]
        
        # Populate the corners and centers if given
        for iLoc in range(len(cLoc)):
            if cLoc[iLoc] is not None:
                ESMP.ESMP_GridAddCoord(self.grid, staggerloc=locs[iLoc])
    
                exLBLoc, exUBLoc = ESMP.ESMP_GridGetCoord(self.grid, 
                                                          locs[iLoc])
                                                        
                xyLoc = []
                for i in range(rank):
                    tmp = ESMP.ESMP_GridGetCoordPtr(self.grid, i+1, locs[iLoc])
                    xyLoc.append(tmp)
                
                # Poplulate the self.grid with coordinates
                for iC in range(2):
                    p = 0
                    for i1 in range(exLBLoc[1], exUBLoc[1]):
                        for i0 in range(exLBLoc[0], exUBLoc[0]):
                            xyLoc[iC][p] = cLoc[iC][i0, i1]
                            p = p + 1

        # Populate the  mask
        ESMP.ESMP_GridAddItem(self.grid, item=ESMP.ESMP_GRIDITEM_MASK)
        self.mask = ESMP.ESMP_GridGetItem(self.grid, 
                                          item=ESMP.ESMP_GRIDITEM_MASK)
        p = 0
        for i1 in range(exLBLoc[1], exUBLoc[1]):
            for i0 in range(exLBLoc[0], exUBLoc[0]):
                if mask is not None:
                    self.mask[p] = mask[i0, i1]
                else:
                    self.mask[p] = 0
                p = p + 1
        
    def __del__(self):
        pass

class EsmfStructMesh:
    """
    Mesh types
                     3                          4 ---------- 3
                    / \                         |            |  
                   /   \                        |            |
                  /     \                       |            |
                 /       \                      |            |
                /         \                     |            |
               1 --------- 2                    1 ---------- 2

           ESMC_MESHELEMTYPE_TRI            ESMC_MESHELEMTYPE_QUAD



                                            
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

       ESMC_MESHELEMTYPE_TETRA             ESMC_MESHELEMTYPE_HEX  

    Create an ESMP mesh(grid) object.
    """
    def __init__(self, coords, meshType = ESMP.ESMP_MESHELEMTYPE_QUAD):
        """
        Creator for an ESMF Mesh
        @param coords list of curvilinear coordinates [xx, yy, ...]
        @param meshType triangle, quad, cube, tetrahedron. See above
                        See ESMP docs for details
        """
        self.numTopoDims = len(coords[0].shape)
        self.numSpaceDims = len(coords)
        self.dimsN = coords[0].shape
        dimsE = list(self.dimsN)
        for i in range(len(dimsE)): dimsE[i] = dimsE[i]-1
        self.dimsE = tuple(dimsE)

        self.numNodes = reduce(operator.mul, self.dimsN)
        self.nodeIndx = np.arange(1, self.numNodes+1, dtype = np.int32)
        self.pes = np.zeros((self.numNodes, ), np.int32)
        self.xyz = np.zeros((self.numNodes, self.numSpaceDims), np.float64)

        for i in range(self.numSpaceDims):
            self.xyz[:, i] = coords[i].reshape(self.xyz[:, i].shape)
        self.numCells = reduce(lambda x, y:(x)*(y), self.dimsE)
        self.cellIndx = np.arange(1, self.numCells+1, dtype = np.int32)
        self.cellTyps = meshType * np.ones((self.numCells, ), np.int32)
        self.cellConn = np.zeros((self.numCells, 2**self.numTopoDims), np.int32)

        # Note: The node ordering for connectivity for the conservative case 
        # must go in counter clockwise order. If you are receiving a rc = 506
        cellConn = []
        for k in range(self.numCells):
            i = k + (k // (self.dimsN[-1] - 1))
            # cell indexing is 1-based (?)
            i1 = i + 1
            genCell = np.array([i1, i1+1, i1+1+self.dimsN[-1], i1+self.dimsN[-1]])
            cellConn.append(genCell)

        self.cellConn = np.array(cellConn, np.int32)
        self.grid = ESMP.ESMP_MeshCreate(self.numTopoDims, self.numSpaceDims)
        ESMP.ESMP_MeshAddNodes(self.grid, self.numNodes, self.nodeIndx,
                               self.xyz, self.pes)
        ESMP.ESMP_MeshAddElements(self.grid, self.numCells, self.cellIndx,
                                  self.cellTyps, self.cellConn)

    def getNodeDims(self):
        """
        Get dimensions of the nodal mesh
        @return array
        """
        return self.dimsN

    def getElemDims(self):
        """
        Get dimensions of the element (cellualr) mesh
        @return array
        """
        return self.dimsE
    
    def writeMesh(self, filename):
        """
        Write the mesh out to a VTK file
        @param filename like it says, the filename
        """
        ESMP.ESMP_MeshWrite(self.grid, filename)

    def  __del__(self):
        ESMP.ESMP_MeshDestroy(self.grid)

class EsmfStructField:
    """
    Create a structured field object
    """
    def __init__(self, esmfGrid, name, data, 
                 meshloc = ESMP.ESMP_MESHLOC_NODE):
        """
        Creator for ESMF Field
        @param esmfGrid instance of an ESMP_Mesh
        @param name field name
        @param data numpy ndarray of data
        @param meshloc ESMP_MESHLOC_NODE for Bilinear interpolation
                       ESMP_MESHLOC_ELEMENT for Conservative interpolation
        """
        locations = [ESMP.ESMP_MESHLOC_NODE, ESMP.ESMP_MESHLOC_ELEMENT]
        if meshloc not in locations:
            raise cdms2.CDMSError, """
                  Coordinate system must be ESMP.ESMP_MESHLOC_NODE
                                            ESMP.ESMP_MESHLOC_ELEMENT"""

        numpyType2EsmfType = {
            'float64': ESMP.ESMP_TYPEKIND_R8,
            'float32': ESMP.ESMP_TYPEKIND_R4,
            'int64': ESMP.ESMP_TYPEKIND_I8,
            'int64': ESMP.ESMP_TYPEKIND_I4, }
        etype = numpyType2EsmfType[str(data.dtype)]
        self.field = ESMP.ESMP_FieldCreate(esmfGrid.grid, name,
                        meshloc = meshloc,
                        typekind = etype)
        # Copy the data
        ptr = self.getPointer()
        ptr[:] = data.flat

    def getPointer(self):
        """
        Get the field pointer to the data
        @return pointer to field data
        """
        pe = 0 # (serial)
        return ESMP.ESMP_FieldGetPtr(self.field, localDe = pe)

    def  __del__(self):
        ESMP.ESMP_FieldDestroy(self.field)

class EsmfGridField(EsmfStructField):
    """
    Create a grid field object. Inherits from EsmfStructField
    """
    def __init__(self, esmfGrid, name, data, 
                 staggerloc = ESMP.ESMP_MESHLOC_NODE):
        """
        Creator for ESMF Field
        @param esmfGrid instance of an ESMP_Mesh
        @param name field name
        @param data numpy ndarray of data
        @param staggerloc ESMP_STAGGERLOC_CENTER
                       ESMP_STAGGERLOC_CORNER
        """
        locations = [ESMP.ESMP_MESHLOC_NODE, ESMP.ESMP_MESHLOC_ELEMENT]
        if meshloc not in locations:
            raise cdms2.CDMSError, """
                  Coordinate system must be ESMP.ESMP_MESHLOC_NODE
                                            ESMP.ESMP_MESHLOC_ELEMENT"""
        numpyType2EsmfType = {
            'float64': ESMP.ESMP_TYPEKIND_R8,
            'float32': ESMP.ESMP_TYPEKIND_R4,
            'int64': ESMP.ESMP_TYPEKIND_I8,
            'int64': ESMP.ESMP_TYPEKIND_I4, }
        etype = numpyType2EsmfType[str(data.dtype)]
        self.field = ESMP.ESMP_FieldCreateGrid(esmfGrid.grid, name,
                        staggerloc = staggerloc,
                        typekind = etype)
        # Copy the data
        ptr = self.getPointer()
        ptr[:] = data.flat

class EsmfRegrid:
    """
    Regrid source grid data to destination grid data
    """
    def __init__(self, srcField, dstField,
                 regridMethod   = ESMP.ESMP_REGRIDMETHOD_BILINEAR,
                 unMappedAction = ESMP.ESMP_UNMAPPEDACTION_ERROR):
        """
        Regrid
        @param srcField the source field object
        @param dstField the destination field object
        @param regridMethod ESMF constanct bilinear, conservative, etc.
        @param unMappedAction ESMP Constant Error, Ignore, etc.
        """
        self.srcField = srcField
        self.dstField = dstField
        self.regrid = ESMP.ESMP_FieldRegridStore( srcField.field, dstField.field,
                                                  regridMethod, unMappedAction)

    def __call__(self, srcField=None, dstField=None):
        """
        Apply interpolation weights
        @param srcField source field (or None if src field passed to constructor is to be used)
        @param dstField destination field (or None if dst field passed to constructor is to be used)
        """
        if srcField == None:
            srcField = self.srcField
        if dstField == None:
            dstField = self.dstField
        ESMP.ESMP_FieldRegrid(srcField.field, dstField.field, self.regrid)
    
    def __del__(self):
        ESMP.ESMP_FieldRegridRelease(self.regrid)

def _createCLMeshFromAxes(lons, lats, conserved = 0):
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
    tlon = np.linspace(lonb, lone, nlon)   # X
    tlat = np.linspace(latb, late, nlat)   # Y
    lon = np.array(tlon, np.float64)
    lat = np.array(tlat, np.float64)

    dimsN = [len(lat), len(lon)]

    lon2D = getTensorProduct(lon, 1, dimsN)
    lat2D = getTensorProduct(lat, 0, dimsN)

    dimsN = lat2D.shape
    dimsE = []
    for i in dimsN: dimsE.append(i-conserved)

    # Cartesian Coordinates
    rad = np.pi/180.0
    XXN = np.cos(lat2D * rad) * np.cos(lon2D * rad)
    YYN = np.cos(lat2D * rad) * np.sin(lon2D * rad)
    ZZN = np.sin(lat2D * rad)

    return (XXN, YYN, ZZN), (dimsN, dimsE), (lon2D, lat2D)

def _createCLMeshFrom2DAxes(lon2D, lat2D):
    """
    Contructor for a mesh using axes
    @param lon2D numpy array of longitudes
    @param lat2D numpy array of latitudes
    @return tuple of xyz coordinates, tuple of dimensions
    """
    dimsN = lon2D.shape
    dimsE = list(dimsN)
    for i in dimsE: dimsE[i] = dimsE[i]-1

    # Cartesian Coordinates
    rad = np.pi/180.0
    XXN = np.cos(lat2D * rad) * np.cos(lon2D * rad)
    YYN = np.cos(lat2D * rad) * np.sin(lon2D * rad)
    ZZN = np.sin(lat2D * rad)

    return (XXN, YYN, ZZN), (dimsN, dimsE)

def getTensorProduct(axis, dim, dims):
    """
    Convert an axis into a curvilinear coordinate by applying
    a tensor product. This could be replaced by numpy.meshgrid for 2D.
    @param axis 1D array of coordinates
    @param dim dimensional index of the above coordinate
    @param dims sizes of all coordinates
    @return coordinate values obtained by tensor product
    """
    return np.outer(np.outer( np.ones(dims[:dim], axis.dtype), axis),
                      np.ones(dims[dim+1:], axis.dtype)).reshape(dims)

def createPlot(srcCds, dstCds, data, vmin = 0, vmax = 0):
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
        pl.title = titles[i]
        pl.colorbar()
    pl.show()

def writeVSH5(filename, grid, data):
    """
    Write a vizschema compliant file for an unstructured mesh
    @param filename location of output data
    @param grid esmf grid object containing xyz coordinates and cell connectivity
    @param data data
    """
    title = 'test'
    h5 = tables.openFile(filename, mode = 'w', title = title)
    pointname = "TestPoints"
    dataname = 'TestData'
    xgroup = h5.createGroup("/", "TestPoints", "P")
    xgroup._f_setAttr("vsType", "mesh")
    xgroup._f_setAttr("vsKind", "unstructured")
    xgroup._f_setAttr("vsQuadrilaterals", "quads")
    carray = h5.createArray("/"+pointname, 'quads', grid.cellConn-1)
    xarray = h5.createArray("/"+pointname, 'points', grid.xyz)

    darray = h5.createArray("/", dataname, data.flat[:])
    darray._f_setAttr("coordinate", "lat lon")
    darray._f_setAttr("vsType", "variable")
    darray._f_setAttr("vsMesh", pointname)
    darray._f_setAttr("vsCentering", "zonal")
    h5.close()

def _create2DMesh(axes, factor):
    """
    Create a 2d, 2d mesh
    @param axes x, y axes
    @param factor for creating data
    """
    y, x = axes
    dimsN = [len(x), len(y)]       # NODES
    dimsE = [len(x)-1, len(y)-1]   # ELEMENTS

    nDimE = reduce(lambda a, b:a*b, dimsE)

    xxN = getTensorProduct(x, 0, dimsN)
    yyN = getTensorProduct(y, 1, dimsN)

    # EMSP Create Mesh
    grid = EsmfStructMesh([yyN, xxN])

    # Data  -- Must be float 64
    dataN = np.array(xxN *factor*yyN, np.float64)
    dataE = np.zeros(dimsE, np.float64)
    for k in range(nDimE):
        i, j = k/dimsE[1], k % dimsE[1]
        dataE[i, j] = i * factor  * 10. + j
    return grid, dataN, dataE

def test2d(useMethod = 0, doPlot = False):
    """
    2d topological
    2d Spatial
    @param useMethod choose between Bilinear (0, default) and Conservative (1)
    """
    print 'Flat World --', method[useMethod]
    unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
    filepref = ['srcGrid', 'dstGrid']
    filename = {}
    for f in filepref: filename[f] = "%s_%s" % (f, method[useMethod])

    snx1 = 230
    sny1 = 115
    snx  = snx1 - 1
    sny  = sny1 -1
    numNodes = snx1*sny1
    numCells = snx * sny

    # Source Grid
    src_x = np.linspace(0, snx1, snx1, np.float64)
    src_y = np.linspace(0, sny1, sny1, np.float64)
    print ' Source'
    srcGrid, srcDataN, srcDataE = _create2DMesh([src_x, src_y], 2)

    ESMP.ESMP_MeshWrite(srcGrid.grid, filename['srcGrid'])

    srcData = [srcDataN, srcDataE]
    srcField = EsmfStructField(srcGrid, 'srcField', srcData[useMethod],
                               meshloc = meshloc[useMethod])

    # Destination Grid
    dnx1 = 360 
    dny1 = 180
    dnx, dny = dnx1-1, dny1-1
    dst_x = np.linspace(0, snx1, dnx1, np.float64)
    dst_y = np.linspace(0, sny1, dny1, np.float64)
    print ' Destination'
    dstGrid, dstDataN, dstDataE = _create2DMesh([dst_x, dst_y], 1)
    dstDataE[:] = -1

    ESMP.ESMP_MeshWrite(dstGrid.grid, filename['dstGrid'])

    dstData = [dstDataN, dstDataE]
    dstField = EsmfStructField(dstGrid, 'dstField', dstData[useMethod],
                               meshloc = meshloc[useMethod])

    # Regrid
    print ' Regrid'
    regrid = EsmfRegrid(srcField, dstField, regridMethod = regridMethod[useMethod],
                unMappedAction = unMappedAction)
    
    # Check the output of the regridding
    sptr = srcField.getPointer()
    dptr = dstField.getPointer()

    print 'Src Cell avg before', srcData[useMethod].sum()/(numCells), snx, sny
    print 'Src Cell avg after ', sptr.sum()/(numCells)
    print 'Dst Cell avg before', dstDataE.sum()/(dnx*dny), dnx, dny
    print 'Dst Cell avg after ', dptr.sum()/(dnx*dny)

    print
    errorRatio = 100*(1 - (sptr.sum()/(numCells))/(dptr.sum()/(dnx*dny)))
    print 'Error Ratio s/d', errorRatio, " %"

    if doPlot:
        sp = np.reshape(sptr, (sny, snx))
        dp = np.reshape(dptr, (dny, dnx))
        np.figure()
        np.subplot(2,2,1)
        np.pcolor(srcData[useMethod], edgecolor = "w")
        np.title('Src Before')
        np.colorbar()
        np.subplot(2,2,2)
        np.pcolor(sp, edgecolor = "w")
        np.title('Src After')
        np.colorbar()
        np.subplot(2,2,3)
        np.pcolor(dstData[useMethod], edgecolor = "w")
        np.title('Dst Before')
        np.colorbar()
        np.subplot(2,2,4)
        np.pcolor(dp, edgecolor = "w")
        np.title('Dst After')
        np.colorbar()
        np.show()

    del regrid
    del dstField
    del srcField
    del srcGrid
    del dstGrid

    print ' Done'

def testCurviLinear(useMethod, writeVTK = False, doPlot = False):
    """
    Create a curvilinear mesh and regrid it.
    topological 2d
    spatial 3d
    @param useMethod choose between Bilinear (0, default) and Conservative (1)
    """
    print '\nCurvilinear Coordinates --', method[useMethod]

    unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
    filepref = ['srcGrid', 'dstGrid']
    filename = {}
    for f in filepref: filename[f] = "%s_%s" % (f, method[useMethod])

    snlon, snlat = 192, 189
    dnlon, dnlat = 360, 180

    print ' Source'
    srcxyz, srcDims, srcCds = _createCLMeshFromAxes((-.93750, 359.0625, snlon), 
                                                   (-80.00, 80, snlat), 
                                                   useMethod)
    sxxn, syyn, szzn = srcxyz
    srcDimsN, srcDimsE = srcDims
    lon2D, lat2D = srcCds

    # Create the grid object
    srcESMFGrid = EsmfStructMesh([sxxn, syyn, szzn])

    # Field Data
    nElem = srcDims[useMethod][0]*srcDims[useMethod][1] 
    srcData = np.ones(srcDims[useMethod], np.float64)

    for j in range(srcDims[useMethod][0]):
        for i in range(srcDims[useMethod][1]): srcData[j, i] = i * j

    if writeVTK: 
        ESMP.ESMP_MeshWrite(srcESMFGrid.grid, filename['srcGrid'])
        writeVSH5('testSrc.vsh5', srcESMFGrid, srcData)

    srcESMFField = EsmfStructField(srcESMFGrid, 'source', srcData, 
                        meshloc = meshloc[useMethod])

    print ' Destination'
    dstxyz, dstDims, dstCds = _createCLMeshFromAxes((-.93750, 359.0625, dnlon), 
                                                   (-80.00, 80.00, dnlat), 
                                                   useMethod)
    dxxn, dyyn, dzzn = dstxyz
    dstDimsN, dstDimsE = dstDims
    lon2D, lat2D = dstCds

    # Create the grid object
    dstESMFGrid = EsmfStructMesh([dxxn, dyyn, dzzn])

    # Field Data
    dstData = np.ones(dstDims[useMethod], np.float64)

    dstESMFField = EsmfStructField(dstESMFGrid, 'source', dstData, 
                        meshloc = meshloc[useMethod])
    if writeVTK: 
        ESMP.ESMP_MeshWrite(dstESMFGrid.grid, filename['dstGrid'])
        writeVSH5('testDst.vsh5', dstESMFGrid, dstData)

    # Interpolate Bilinear
    regrid = EsmfRegrid(srcESMFField, dstESMFField,
                regridMethod = regridMethod[useMethod],
                unMappedAction = unMappedAction)

    newSrc = np.reshape(srcESMFField.getPointer(), srcDims[useMethod])
    newDst = np.reshape(dstESMFField.getPointer(), dstDims[useMethod])
    
    if writeVTK: 
        ESMP.ESMP_MeshWrite(dstESMFGrid.grid, filename['dstGrid'])
        writeVSH5('testDst.vsh5', dstESMFGrid, newDst)

    srcval = newSrc.sum()/newSrc.size
    dstval = newDst.sum()/newDst.size
    print '     Source Data/cell', srcval
    print 'Destination Data/cell', dstval
    print 'src/dst, src-dst   ', 100*(1-srcval/dstval), "%", srcval-dstval

    srcCds = np.meshgrid(np.linspace(0, 360, snlon-1), 
                         np.linspace(-90, 87.712616, snlat-1))
    dstCds = np.meshgrid(np.linspace(0, 360, dnlon-1), 
                         np.linspace(-90, 87.712616, dnlat-1))
    if doPlot: 
        createPlot(srcCds, dstCds, (srcData, newSrc, 
                   dstData, newDst))

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
    test2d(0, doPlot = False)  # Flat world
    test2d(1, doPlot = False)  # Flat world
    testCurviLinear(0, writeVTK = False, doPlot = False)  # Curvilinear world
    testCurviLinear(1, writeVTK = False, doPlot = False)  # Curvilinear world
    ESMP.ESMP_Finalize()
