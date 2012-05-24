import ESMP
import numpy
import operator
import cdms2
import tables
from regrid2 import RegridError

# Global variables
method = ['bilinear', 'conservative']
meshlocList = [ESMP.ESMP_MESHLOC_NODE, ESMP.ESMP_MESHLOC_ELEMENT]
staggerlocList = [ESMP.ESMP_STAGGERLOC_CENTER, ESMP.ESMP_STAGGERLOC_CORNER]
regridMethodList = [ESMP.ESMP_REGRIDMETHOD_BILINEAR, 
                    ESMP.ESMP_REGRIDMETHOD_CONSERVE]
coordSystemsList = [ESMP.ESMP_COORDSYS_CART, ESMP.ESMP_COORDSYS_SPH_DEG,
                    ESMP.ESMP_COORDSYS_SPH_RAD]

class EsmfStructGrid:
    """
    Create an ESMP Grid
    """
    def __init__(self, maxIndex, coordSys = ESMP.ESMP_COORDSYS_SPH_DEG, 
                 periodicity = 0):
        """
        Constructor ESMP Grid
        @param maxIndex    The indices of the size of the array
        @param coordSys    coordinate system
                           ESMP.ESMP_COORDSYS_CART              Cartesian
                           ESMP.ESMP_COORDSYS_SPH_DEG (default) Degrees
                           ESMP.ESMP_COORDSYS_SPH_RAD           Radians
        @param periodicity Does the grid have a periodic coordinate
                           0 No periodicity
                           1 Periodic in x axis
                           2 Periodic in x, y axes

        """
        self.grid = None
        if isinstance(maxIndex, list):
            self.maxIndex = numpy.ndarray(maxIndex, dtype = numpy.int32)
        elif isinstance(maxIndex, numpy.ndarray):
            if str(maxIndex.dtype) != 'int32':
                self.maxIndex = numpy.ndarray(maxIndex, dtype = numpy.int32)
            else:
                self.maxIndex = maxIndex
        else:
            string = "maxIndex must be a list of numpy.ndarray of dtype = int32"
            raise RegridError, string
                
        # Initialize
        self.maskValues = None

        if periodicity == 0:
            self.grid = ESMP.ESMP_GridCreateNoPeriDim(self.maxIndex, 
                                                      coordSys = coordSys)
        elif periodicity == 1:
            self.grid = ESMP.ESMP_GridCreate1PeriDim(self.maxIndex, 
                                                     coordSys = coordSys)
        elif periodicity == 2:
            self.grid = ESMP.ESMP_GridCreate2PeriDim(self.maxIndex, 
                                                     coordSys = coordSys)
        else:
            raise RegridError, "Periodic dimensions > 2 not permitted."
    
    def addCoords(self, coords, staggerloc = ESMP.ESMP_STAGGERLOC_CENTER):
        """
        Populate the grid with cell centers or corners. The coordinates can be
        coordinates (centers) or bounds (corners)
        @param coords   The coordinates of for the grid. List of numpy arrays
        @param staggerloc  The stagger location
                           ESMP.ESMP_STAGGERLOC_CENTER (default)
                           ESMP.ESMP_STAGGERLOC_CORNER
        Note: coord dims in cdms2 are ordered in y, x, but ESMF expect x, y,
        hence the dimensions are reversed here. If you are receiving unexpected
        results, try reversing the order of coordinate dimensions.
        """
        rank = len(coords[0].shape)
        
        # Copy the data
        ESMP.ESMP_GridAddCoord(self.grid, staggerloc=staggerloc)

        for i in range(rank):
            ptr = ESMP.ESMP_GridGetCoordPtr(self.grid, i+1, staggerloc)
            
            # Poplulate the self.grid with coordinates or the bounds as needed
            # numpy.arrays required since numpy.ma arrays don't support flat
            ptr[:] = numpy.array(coords[i]).flat

    def addArea(self, area):
        """
        Add the area
        @param area numpy array. 
        """
        ESMP.ESMP_GridAddItem(self.grid, item=ESMP.ESMP_GRIDITEM_AREA)
        areaPtr = ESMP.ESMP_GridGetItem(self.grid, 
                                      item=ESMP.ESMP_GRIDITEM_AREA)
        areaPtr[:] = area.flat
        
    def getArea(self):
        """
        @return a area pointer
        """
        areaPtr = ESMP.ESMP_GridGetItem(self.grid, 
                                        item = ESMP.ESMP_GRIDITEM_AREA)
        return numpy.reshape(areaPtr, self.maxIndex)

    def addMask(self, mask):
        """
        Add the mask
        @param mask numpy array. 1 is invalid by default, but can be 
                    overridden with the maskValues argument
        """
        ESMP.ESMP_GridAddItem(self.grid, item=ESMP.ESMP_GRIDITEM_MASK)
        maskPtr = ESMP.ESMP_GridGetItem(self.grid, 
                                        item=ESMP.ESMP_GRIDITEM_MASK)
        maskPtr[:] = mask.flat
        
    def getMask(self):
        """
        @return a mask pointer
        """
        maskPtr = ESMP.ESMP_GridGetItem(self.grid, 
                                        item = ESMP.ESMP_GRIDITEM_MASK)
        return numpy.reshape(maskPtr, self.shape)

    def getPointer(self, dim, staggerloc):
        """
        Return the coordinates for a dimension
        @param dim desired dimension 1-based
        @param staggerloc Stagger location
        """
        if dim < 1:
            raise RegridError, "dim is 1-based"
        
        gridPtr = ESMP.ESMP_GridGetCoordPtr(self.grid, dim, staggerloc)
        return gridPtr


    def __del__(self):
        ESMP.ESMP_GridDestroy(self.grid)

class EsmfStructField:
    """
    Create a grid field object.
    """
    def __init__(self, esmfGrid, name, data = None, 
                 staggerloc = ESMP.ESMP_STAGGERLOC_CENTER):
        """
        Creator for ESMF Field
        @param esmfGrid instance of an ESMP_Grid
        @param name field name
        @param data numpy ndarray of data
        @param staggerloc ESMP_STAGGERLOC_CENTER
                          ESMP_STAGGERLOC_CORNER
        """
        if staggerloc not in staggerlocList:
            raise cdms2.CDMSError, """
                  Grid staggering must be ESMP.ESMP_STAGGERLOC_CENTER
                                          ESMP.ESMP_STAGGERLOC_CORNER"""
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

    def getArea(self, areaField):
        """
        Compare Source mass to Destinatin mass
        """
        ESMP.ESMP_FieldRegridGetArea(areaField.field)
        areaPtr = areaField.getPointer()
        return areaPtr

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
#        ESMP.ESMP_FieldRegridRelease(self.regridHandle)
    
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

#def _createCLMeshFrom2DAxes(lon2D, lat2D):
#    """
#    Contructor for a mesh using axes
#    @param lon2D numpy array of longitudes
#    @param lat2D numpy array of latitudes
#    @return tuple of xyz coordinates, tuple of dimensions
#    """
#    dimsN = lon2D.shape
#    dimsE = list(dimsN)
#    for i in dimsE: dimsE[i] = dimsE[i]-1
#
#    # Cartesian Coordinates
#    rad = numpy.pi/180.0
#    XXN = numpy.cos(lat2D * rad) * numpy.cos(lon2D * rad)
#    YYN = numpy.cos(lat2D * rad) * numpy.sin(lon2D * rad)
#    ZZN = numpy.sin(lat2D * rad)
#
#    return (XXN, YYN, ZZN), (dimsN, dimsE)
#
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

#def _create2DMesh(axes, factor):
#    """
#    Create a 2d, 2d mesh
#    @param axes x, y axes
#    @param factor for creating data
#    """
#    y, x = axes
#    dimsN = [len(x), len(y)]       # NODES
#    dimsE = [len(x)-1, len(y)-1]   # ELEMENTS
#
#    nDimE = reduce(lambda a, b:a*b, dimsE)
#
#    xxN = getTensorProduct(x, 0, dimsN)
#    yyN = getTensorProduct(y, 1, dimsN)
#
#    # EMSP Create Mesh
#    grid = EsmfStructMesh([yyN, xxN])
#
#    # Data  -- Must be float 64
#    dataN = numpy.array(xxN *factor*yyN, numpy.float64)
#    dataE = numpy.zeros(dimsE, numpy.float64)
#    for k in range(nDimE):
#        i, j = k/dimsE[1], k % dimsE[1]
#        dataE[i, j] = i * factor  * 10. + j
#    return grid, dataN, dataE
#
#def test2d(useMethod = 0, doPlot = False):
#    """
#    2d topological
#    2d Spatial
#    @param useMethod choose between Bilinear (0, default) and Conservative (1)
#    """
#    print 'Flat World --', method[useMethod]
#    unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
#    filepref = ['srcGrid', 'dstGrid']
#    filename = {}
#    for f in filepref: filename[f] = "%s_%s" % (f, method[useMethod])
#
#    snx1 = 230
#    sny1 = 115
#    snx  = snx1 - 1
#    sny  = sny1 -1
#    #numNodes = snx1*sny1
#    numCells = snx * sny
#
#    # Source Grid
#    src_x = numpy.linspace(0, snx1, snx1, numpy.float64)
#    src_y = numpy.linspace(0, sny1, sny1, numpy.float64)
#    print ' Source'
#    srcGrid, srcDataN, srcDataE = _create2DMesh([src_x, src_y], 2)
#
#    ESMP.ESMP_MeshWrite(srcGrid.grid, filename['srcGrid'])
#
#    srcData = [srcDataN, srcDataE]
#    srcField = EsmfStructField(srcGrid, 'srcField', srcData[useMethod],
#                               meshloc = meshlocList[useMethod])
#
#    # Destination Grid
#    dnx1 = 360 
#    dny1 = 180
#    dnx, dny = dnx1-1, dny1-1
#    dst_x = numpy.linspace(0, snx1, dnx1, numpy.float64)
#    dst_y = numpy.linspace(0, sny1, dny1, numpy.float64)
#    print ' Destination'
#    dstGrid, dstDataN, dstDataE = _create2DMesh([dst_x, dst_y], 1)
#    dstDataE[:] = -1
#
#    ESMP.ESMP_MeshWrite(dstGrid.grid, filename['dstGrid'])
#
#    dstData = [dstDataN, dstDataE]
#    dstField = EsmfStructField(dstGrid, 'dstField', dstData[useMethod],
#                               meshloc = meshlocList[useMethod])
#
#    # Regrid
#    print ' Regrid'
#    regrid = EsmfRegrid(srcField, dstField, 
#                regridMethod = regridMethodList[useMethod],
#                unMappedAction = unMappedAction)
#    
#    # Check the output of the regridding
#    sptr = srcField.getPointer()
#    dptr = dstField.getPointer()
#
#    print 'Src Cell avg before', srcData[useMethod].sum()/(numCells), snx, sny
#    print 'Src Cell avg after ', sptr.sum()/(numCells)
#    print 'Dst Cell avg before', dstDataE.sum()/(dnx*dny), dnx, dny
#    print 'Dst Cell avg after ', dptr.sum()/(dnx*dny)
#
#    print
#    errorRatio = 100*(1 - (sptr.sum()/(numCells))/(dptr.sum()/(dnx*dny)))
#    print 'Error Ratio s/d', errorRatio, " %"
#
#    import matplotlib.pylab as pl
#
#    if doPlot:
#        sp = numpy.reshape(sptr, (sny, snx))
#        dp = numpy.reshape(dptr, (dny, dnx))
#        pl.figure()
#        pl.subplot(2,2,1)
#        pl.pcolor(srcData[useMethod], edgecolor = "w")
#        pl.title('Src Before')
#        pl.colorbar()
#        pl.subplot(2,2,2)
#        pl.pcolor(sp, edgecolor = "w")
#        pl.title('Src After')
#        pl.colorbar()
#        pl.subplot(2,2,3)
#        pl.pcolor(dstData[useMethod], edgecolor = "w")
#        pl.title('Dst Before')
#        pl.colorbar()
#        pl.subplot(2,2,4)
#        pl.pcolor(dp, edgecolor = "w")
#        pl.title('Dst After')
#        pl.colorbar()
#        pl.show()
#
#    del regrid
#    del dstField
#    del srcField
#    del srcGrid
#    del dstGrid
#
#    print ' Done'
#

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

    unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
    filepref = ['srcGrid', 'dstGrid']
    filename = {}
    for f in filepref: filename[f] = "%s_%s" % (f, method[useMethod])

    snlon, snlat = 10, 20
    dnlon, dnlat =  5, 10

    print ' Source'
    srcxyz, srcDims, srcCds = _createCLGridFromAxes((-.93750, 359.0625, snlon), 
                                                   (-80.00, 80, snlat), 
                                                   useMethod)
    # Create the grid object
    maxIndex = numpy.array(srcDims[0], dtype=numpy.int32)
    print maxIndex
    srcESMFGrid = EsmfStructGrid(maxIndex)
    srcESMFGrid.addCoords(srcCds)

    # Field Data
    srcData = numpy.ones(srcDims[useMethod], numpy.float64)

    for j in range(srcDims[useMethod][0]):
        for i in range(srcDims[useMethod][1]): srcData[j, i] = i * j

    if writeVTK: 
        ESMP.ESMP_MeshWrite(srcESMFGrid.grid, filename['srcGrid'])
        writeVSH5('testSrc.vsh5', srcESMFGrid, srcData)

    srcESMFField = EsmfStructField(srcESMFGrid, 'source', srcData, 
                       staggerloc = staggerlocList[useStagger])

    print ' Destination'
    dstxyz, dstDims, dstCds = _createCLGridFromAxes((-.93750, 359.0625, dnlon), 
                                                   (-80.00, 80.00, dnlat), 
                                                   useMethod)

    # Create the grid object
    maxIndex = numpy.array(dstDims[0], dtype=numpy.int32)
    dstESMFGrid = EsmfStructGrid(maxIndex)
    dstESMFGrid.addCoords(dstCds)

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
        fileName = "%s_%s_test.png" % (regridMethodList[useMethod], staggerlocList[useStagger])
        createPlot(srcCds, dstCds, (srcData, newSrc, 
                   dstData, newDst), fileName = fileName, savefig = savefig)

    del regrid
    del dstESMFField
    del srcESMFField
    del srcESMFGrid
    del dstESMFGrid

    print ' Done'

#def testCurviLinearMesh(useMethod, writeVTK = False, doPlot = False):
#    """
#    Create a curvilinear mesh and regrid it.
#    topological 2d
#    spatial 3d
#    @param useMethod choose between Bilinear (0, default) and Conservative (1)
#    @param writeVTK Write out a vtk file for use in VisIt
#    @param doPlot a X plot
#    """
#    print '\nCurvilinear Coordinates --', method[useMethod]
#
#    unMappedAction = ESMP.ESMP_UNMAPPEDACTION_IGNORE
#    filepref = ['srcGrid', 'dstGrid']
#    filename = {}
#    for f in filepref: filename[f] = "%s_%s" % (f, method[useMethod])
#
#    snlon, snlat = 192, 189
#    dnlon, dnlat = 360, 180
#
#    print ' Source'
#    srcxyz, srcDims, srcCds = _createCLMeshFromAxes((-.93750, 359.0625, snlon), 
#                                                   (-80.00, 80, snlat), 
#                                                   useMethod)
#    sxxn, syyn, szzn = srcxyz
#    lon2D, lat2D = srcCds
#
#    # Create the grid object
#    srcESMFGrid = EsmfStructMesh([sxxn, syyn, szzn])
#
#    # Field Data
#    srcData = numpy.ones(srcDims[useMethod], numpy.float64)
#
#    for j in range(srcDims[useMethod][0]):
#        for i in range(srcDims[useMethod][1]): srcData[j, i] = i * j
#
#    if writeVTK: 
#        ESMP.ESMP_MeshWrite(srcESMFGrid.grid, filename['srcGrid'])
#        writeVSH5('testSrc.vsh5', srcESMFGrid, srcData)
#
#    srcESMFField = EsmfStructField(srcESMFGrid, 'source', srcData, 
#                        meshloc = meshlocList[useMethod])
#
#    print ' Destination'
#    dstxyz, dstDims, dstCds = _createCLMeshFromAxes((-.93750, 359.0625, dnlon), 
#                                                   (-80.00, 80.00, dnlat), 
#                                                   useMethod)
#    dxxn, dyyn, dzzn = dstxyz
#
#    # Create the grid object
#    dstESMFGrid = EsmfStructMesh([dxxn, dyyn, dzzn])
#
#    # Field Data
#    dstData = numpy.ones(dstDims[useMethod], numpy.float64)
#
#    dstESMFField = EsmfStructField(dstESMFGrid, 'source', dstData, 
#                        meshloc = meshlocList[useMethod])
#    if writeVTK: 
#        ESMP.ESMP_MeshWrite(dstESMFGrid.grid, filename['dstGrid'])
#        writeVSH5('testDst.vsh5', dstESMFGrid, dstData)
#
#    # Interpolate Bilinear
#    regrid = EsmfRegrid(srcESMFField, dstESMFField,
#                regridMethod = regridMethodList[useMethod],
#                unMappedAction = unMappedAction)
#
#    newSrc = numpy.reshape(srcESMFField.getPointer(), srcDims[useMethod])
#    newDst = numpy.reshape(dstESMFField.getPointer(), dstDims[useMethod])
#    
#    if writeVTK: 
#        ESMP.ESMP_MeshWrite(dstESMFGrid.grid, filename['dstGrid'])
#        writeVSH5('testDst.vsh5', dstESMFGrid, newDst)
#
#    srcval = newSrc.sum()/newSrc.size
#    dstval = newDst.sum()/newDst.size
#    print '     Source Data/cell', srcval
#    print 'Destination Data/cell', dstval
#    print 'src/dst, src-dst   ', 100*(1-srcval/dstval), "%", srcval-dstval
#
#    srcCds = numpy.meshgrid(numpy.linspace(0, 360, snlon-1), 
#                         numpy.linspace(-90, 87.712616, snlat-1))
#    dstCds = numpy.meshgrid(numpy.linspace(0, 360, dnlon-1), 
#                         numpy.linspace(-90, 87.712616, dnlat-1))
#    if doPlot: 
#        createPlot(srcCds, dstCds, (srcData, newSrc, 
#                   dstData, newDst))
#
#    del regrid
#    del dstESMFField
#    del srcESMFField
#    del srcESMFGrid
#    del dstESMFGrid
#
#    print ' Done'

if __name__ == "__main__":
    """
    The test allow for a vtk file to be written as well as plots to be created.
    These features are off (False) by default.
    """
    ESMP.ESMP_Initialize()
#    test2d(0, doPlot = False)  # Flat world
#    test2d(1, doPlot = False)  # Flat world
    testCurviLinearGrid(0, 0,writeVTK = False, doPlot = True, savefig = False)  # Curvilinear world
    ESMP.ESMP_Finalize()
