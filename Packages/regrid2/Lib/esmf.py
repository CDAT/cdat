import ESMP
import numpy
import operator
import cdms2
import tables
from regrid2 import RegridError

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
    
class EsmfStructGrid:
    """
    Create an ESMF structured grid
    """
    def __init__(self, coords, bounds = None, mask = None, periodicity = 0, 
                 coordSys = ESMP.ESMP_COORDSYS_SPH_DEG):
        """
        Construct an ESMP Grid object
        @param coords tuple or list of coordinates of the grid (lat, lon)
        @param bounds tuple or list containing the bounds. Only needed if doing
                      conservative interpolation
        @param mask Mask 1 is invalid data 0 valid (numpy definition)
        @param periodicity Number of periodic boundaries
                           0 - None Default
                           1 - One e.g. longitude (Assume global)
                           2 - Two e.g.
        @param coordSys    ESMP.ESMP_COORDSYS_CART
                           ESMP.ESMP_COORDSYS_SPH_DEG (default)
                           ESMP.ESMP_COORDSYS_SPH_RAD
        """
        coordSystems = [ESMP.ESMP_COORDSYS_CART, ESMP.ESMP_COORDSYS_SPH_DEG,
                        ESMP.ESMP_COORDSYS_SPH_RAD]
        if coordSys not in coordSystems:
            raise cdms2.CDMSError, """
                  Coordinate system must be ESMP.ESMP_COORDSYS_CART
                                            ESMP.ESMP_COORDSYS_SPH_DEG
                                            ESMP.ESMP_COORDSYS_SPH_RAD"""
        
        # Make sure there are bounds
        #if bounds is None:
        #    raise RegridError, 'Need the bounds for ESMP.'

        # maxIndex -> x, y 
        self.maxIndex = numpy.array(coords[0].shape[::-1], dtype = numpy.int32)
        rank = len(coords[0].shape)
        
        # Create the grid object
        if periodicity == 0:
            self.grid = ESMP.ESMP_GridCreateNoPeriDim(self.maxIndex, 
                                                      coordSys = coordSys)
        elif periodicity == 1:                                             
            self.grid = ESMP.ESMP_GridCreate1PeriDim(self.maxIndex, 
                                                      coordSys = coordSys)
        else:
            raise cdms2.CDMSError, """
                    Periodicity must be 0, 1 or 2"""

        # Populate the corners and centers
        if bounds is not None:
            staggerLocations = [ESMP.ESMP_STAGGERLOC_CORNER, 
                                ESMP.ESMP_STAGGERLOC_CENTER]
        else:
            staggerLocations = [ESMP.ESMP_STAGGERLOC_CENTER]

        # Copy the data
        for sLoc in staggerLocations:
            ESMP.ESMP_GridAddCoord(self.grid, staggerloc=sLoc)
            exLBLoc, exUBLoc = ESMP.ESMP_GridGetCoord(self.grid, sLoc) 
            
            for i in range(rank):
                ptr = ESMP.ESMP_GridGetCoordPtr(self.grid, i+1, sLoc)
            
            # Poplulate the self.grid with coordinates or the bounds as needed
            # numpy.arrays required since numpy.ma arrays don't support flat
                if sLoc == ESMP.ESMP_STAGGERLOC_CORNER:
                    ptr[:] = numpy.array(bounds[i]).flat
                else:
                    ptr[:] = numpy.array(coords[i]).flat

            # Populate the mask on Cell Centers
            self.maskPtr = None
            if mask is not None and sLoc == ESMP.ESMP_STAGGERLOC_CENTER:
                ESMP.ESMP_GridAddItem(self.grid, item=ESMP.ESMP_GRIDITEM_MASK)
                self.maskPtr = ESMP.ESMP_GridGetItem(self.grid, 
                                              item=ESMP.ESMP_GRIDITEM_MASK)
                self.maskPtr[:] = mask.flat

    def getMask(self):
        """
        @return a mask pointer
        """
        self.maskPtr = ESMP.ESMP_GridGetItem(self.grid, 
                                         item = ESMP.ESMP_GRIDITEM_MASK)
        return numpy.reshape(self.maskPtr, self.maxIndex)

    def getPointer(self, dim):
        """
        Return the coordinates for a dimension
        @param dim desired dimension 1-based
        """
        if dim < 1:
            raise RegridError, "dim is 1-based"

        
        gridPtr = ESMP.ESMP_GridGetCoordPtr(self.grid, dim, self.grid.staggerloc)
        return numpy.reshape(gridPtr, self.maxIndex)

    def __del__(self):
        pass
        #ESMP.EMSP_GridDestroy(self)

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
        self.nodeIndx = numpy.arange(1, self.numNodes+1, dtype = numpy.int32)
        self.pes = numpy.zeros((self.numNodes, ), numpy.int32)
        self.xyz = numpy.zeros((self.numNodes, self.numSpaceDims), numpy.float64)

        for i in range(self.numSpaceDims):
            self.xyz[:, i] = coords[i].reshape(self.xyz[:, i].shape)
        self.numCells = reduce(lambda x, y:(x)*(y), self.dimsE)
        self.cellIndx = numpy.arange(1, self.numCells+1, dtype = numpy.int32)
        self.cellTyps = meshType * numpy.ones((self.numCells, ), numpy.int32)
        self.cellConn = numpy.zeros((self.numCells, 2**self.numTopoDims), numpy.int32)

        # Note: The node ordering for connectivity for the conservative case 
        # must go in counter clockwise order. If you are receiving a rc = 506
        cellConn = []
        for k in range(self.numCells):
            i = k + (k // (self.dimsN[-1] - 1))
            # cell indexing is 1-based (?)
            i1 = i + 1
            genCell = numpy.array([i1, i1+1, i1+1+self.dimsN[-1], i1+self.dimsN[-1]])
            cellConn.append(genCell)

        self.cellConn = numpy.array(cellConn, numpy.int32)
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
                  mesh location must be ESMP.ESMP_MESHLOC_NODE
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

class EsmfGridField:
    """
    Create a grid field object. Inherits from EsmfStructField
    """
    def __init__(self, esmfGrid, name, data, 
                 staggerloc = ESMP.ESMP_STAGGERLOC_CENTER):
        """
        Creator for ESMF Field
        @param esmfGrid instance of an ESMP_Grid
        @param name field name
        @param data numpy ndarray of data
        @param staggerloc ESMP_STAGGERLOC_CENTER
                          ESMP_STAGGERLOC_CORNER
        """
        locations = [ESMP.ESMP_STAGGERLOC_CENTER, ESMP.ESMP_STAGGERLOC_CORNER]
        if staggerloc not in locations:
            raise cdms2.CDMSError, """
                  Grid staggering must be ESMP.ESMP_STAGGERLOC_CENTER
                                          ESMP.ESMP_STAGGERLOC_CORNER"""
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

    def getPointer(self):
        """
        Get the field pointer to the data
        @return pointer to field data
        """
        pe = 0 # (serial)
        return ESMP.ESMP_FieldGetPtr(self.field, localDe = pe)

    def  __del__(self):
        ESMP.ESMP_FieldDestroy(self.field)

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
        @param srcField the source field object
        @param dstField the destination field object

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

        def checkMaskValues(maskValues):
            """ 
            Check type(maskValues), convert to ndarray, dtype = int32
            @param maskValues list or ndarray of mask values
            @return ndarray of dtype = int32, or None if None
            """

            if maskValues is None:
                maskValue = None
            elif isinstance(maskValues, list):
                maskValue = numpy.array(srcMaskValues, dtype = numpy.int32)
            elif isinstance(maskValues, numpy.ndarray):
                maskValue = maskValues.copy()
            else:
                raise RegridError, 'Masked values must be None, a list or numpy array'
            return maskValue

        srcMaskValueArr = checkMaskValues(srcMaskValues)
        dstMaskValueArr = checkMaskValues(dstMaskValues)

        self.regridHandle = ESMP.ESMP_FieldRegridStore( 
                                     srcField.field, 
                                     dstField.field,
                                     srcMaskValues = srcMaskValueArr, 
                                     dstMaskValues = dstMaskValueArr,
                                     srcFracField = srcFrac, 
                                     dstFracField = dstFrac,
                                     regridmethod = regridMethod, 
                                     unmappedaction = unMappedAction)

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
        ESMP.ESMP_FieldRegrid(srcField.field, dstField.field, self.regridHandle)
        ESMP.ESMP_FieldRegridRelease(self.regridHandle)
    
    def __del__(self):
        #ESMP.ESMP_FieldRegridRelease(self.regridHandle)
        pass

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
    rad = numpy.pi/180.0
    XXN = numpy.cos(lat2D * rad) * numpy.cos(lon2D * rad)
    YYN = numpy.cos(lat2D * rad) * numpy.sin(lon2D * rad)
    ZZN = numpy.sin(lat2D * rad)

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
    return numpy.outer(numpy.outer( numpy.ones(dims[:dim], axis.dtype), axis),
                      numpy.ones(dims[dim+1:], axis.dtype)).reshape(dims)

def createPlot(srcCds, dstCds, data, vmin = 0, vmax = 0, savefig = False, fileName = None):
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
    dataN = numpy.array(xxN *factor*yyN, numpy.float64)
    dataE = numpy.zeros(dimsE, numpy.float64)
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
    src_x = numpy.linspace(0, snx1, snx1, numpy.float64)
    src_y = numpy.linspace(0, sny1, sny1, numpy.float64)
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
    dst_x = numpy.linspace(0, snx1, dnx1, numpy.float64)
    dst_y = numpy.linspace(0, sny1, dny1, numpy.float64)
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
        sp = numpy.reshape(sptr, (sny, snx))
        dp = numpy.reshape(dptr, (dny, dnx))
        numpy.figure()
        numpy.subplot(2,2,1)
        numpy.pcolor(srcData[useMethod], edgecolor = "w")
        numpy.title('Src Before')
        numpy.colorbar()
        numpy.subplot(2,2,2)
        numpy.pcolor(sp, edgecolor = "w")
        numpy.title('Src After')
        numpy.colorbar()
        numpy.subplot(2,2,3)
        numpy.pcolor(dstData[useMethod], edgecolor = "w")
        numpy.title('Dst Before')
        numpy.colorbar()
        numpy.subplot(2,2,4)
        numpy.pcolor(dp, edgecolor = "w")
        numpy.title('Dst After')
        numpy.colorbar()
        numpy.show()

    del regrid
    del dstField
    del srcField
    del srcGrid
    del dstGrid

    print ' Done'

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
    print '\nCurvilinear Coordinates --', method[useMethod]
    stagger  = [ESMP.ESMP_STAGGERLOC_CENTER, ESMP.ESMP_STAGGERLOC_CORNER]
    stagName = ['center', 'corner']

    unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
    filepref = ['srcGrid', 'dstGrid']
    filename = {}
    for f in filepref: filename[f] = "%s_%s" % (f, method[useMethod])

    snlon, snlat = 10, 20
    dnlon, dnlat =  5, 10

    print ' Source'
    srcxyz, srcDims, srcCds = _createCLMeshFromAxes((-.93750, 359.0625, snlon), 
                                                   (-80.00, 80, snlat), 
                                                   useMethod)
    sxxn, syyn, szzn = srcxyz
    srcDimsN, srcDimsE = srcDims
    lon2D, lat2D = srcCds

    # Create the grid object
    srcESMFGrid = EsmfStructGrid(srcCds)

    # Field Data
    nElem = srcDims[useMethod][0]*srcDims[useMethod][1] 
    srcData = numpy.ones(srcDims[useMethod], numpy.float64)

    for j in range(srcDims[useMethod][0]):
        for i in range(srcDims[useMethod][1]): srcData[j, i] = i * j

    if writeVTK: 
        ESMP.ESMP_MeshWrite(srcESMFGrid.grid, filename['srcGrid'])
        writeVSH5('testSrc.vsh5', srcESMFGrid, srcData)

    srcESMFField = EsmfGridField(srcESMFGrid, 'source', srcData, 
                       staggerloc = stagger[useStagger])

    print ' Destination'
    dstxyz, dstDims, dstCds = _createCLMeshFromAxes((-.93750, 359.0625, dnlon), 
                                                   (-80.00, 80.00, dnlat), 
                                                   useMethod)
    dxxn, dyyn, dzzn = dstxyz
    dstDimsN, dstDimsE = dstDims
    lon2D, lat2D = dstCds

    # Create the grid object
    dstESMFGrid = EsmfStructGrid(dstCds)

    # Field Data
    dstData = numpy.ones(dstDims[useMethod], numpy.float64)

    dstESMFField = EsmfGridField(dstESMFGrid, 'source', dstData, 
                        staggerloc = stagger[useStagger])
    if writeVTK: 
        ESMP.ESMP_MeshWrite(dstESMFGrid.grid, filename['dstGrid'])
        writeVSH5('testDst.vsh5', dstESMFGrid, dstData)

    # Interpolate Bilinear
    regrid = EsmfRegrid(srcESMFField, dstESMFField,
                regridMethod = regridMethod[useMethod],
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
        fileName = "%s_%s_test.png" % (method[useMethod], stagger[useStagger])
        createPlot(srcCds, dstCds, (srcData, newSrc, 
                   dstData, newDst), fileName = fileName, savefig = savefig)

    del regrid
    del dstESMFField
    del srcESMFField
    del srcESMFGrid
    del dstESMFGrid

    print ' Done'

def testCurviLinearMesh(useMethod, writeVTK = False, doPlot = False):
    """
    Create a curvilinear mesh and regrid it.
    topological 2d
    spatial 3d
    @param useMethod choose between Bilinear (0, default) and Conservative (1)
    @param writeVTK Write out a vtk file for use in VisIt
    @param doPlot a X plot
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
    srcData = numpy.ones(srcDims[useMethod], numpy.float64)

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
    dstData = numpy.ones(dstDims[useMethod], numpy.float64)

    dstESMFField = EsmfStructField(dstESMFGrid, 'source', dstData, 
                        meshloc = meshloc[useMethod])
    if writeVTK: 
        ESMP.ESMP_MeshWrite(dstESMFGrid.grid, filename['dstGrid'])
        writeVSH5('testDst.vsh5', dstESMFGrid, dstData)

    # Interpolate Bilinear
    regrid = EsmfRegrid(srcESMFField, dstESMFField,
                regridMethod = regridMethod[useMethod],
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
#    test2d(0, doPlot = False)  # Flat world
#    test2d(1, doPlot = False)  # Flat world
    # Curvilinear Mesh or grid. Bilinear (0) or Conservative (1)
#    testCurviLinearMesh(0, writeVTK = False, doPlot = False)  # Curvilinear world
#    testCurviLinearMesh(1, writeVTK = False, doPlot = False)  # Curvilinear world
    testCurviLinearGrid(0, 0,writeVTK = False, doPlot = True, savefig = False)  # Curvilinear world
#    testCurviLinearGrid(1, writeVTK = False, doPlot = False)  # Curvilinear world
    ESMP.ESMP_Finalize()
