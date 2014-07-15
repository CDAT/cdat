## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"""CDMS Grid objects"""
import types
import re
from error import CDMSError
import numpy #, PropertiedClasses, internattr
# import regrid2._regrid
import copy, string, sys
from cdmsobj import CdmsObj
from axis import TransientAxis, createAxis, createUniformLatitudeAxis, createUniformLongitudeAxis, getAutoBounds, createGaussianAxis, lookupArray, isSubsetVector
import cdtime

MethodNotImplemented = "Method not yet implemented"

_classifyGrids = 1                      # Determine the type of grid from the grid classifier, not .grid_type

# Set grid classifiction mode. If on, gridtype is determined by the grid
# classification method, regardless of the value of grid.getType().
# (if any). If 'off', the value of .grid_type overrides the classification.
def setClassifyGrids(mode):
    global _classifyGrids
    if mode=='on':
        _classifyGrids=1
    elif mode=='off':
        _classifyGrids=0

# Create a transient rectilinear grid
def createRectGrid(lat, lon, order="yx", type="generic", mask=None):
    return TransientRectGrid(lat, lon, order, type, mask)

# Generate a uniform rectilinear grid
def createUniformGrid(startLat, nlat, deltaLat, startLon, nlon, deltaLon, order="yx", mask=None):
    lat = createUniformLatitudeAxis(startLat, nlat, deltaLat)
    lon = createUniformLongitudeAxis(startLon, nlon, deltaLon)
    return createRectGrid(lat,lon,order,"uniform",mask)

# Generate a grid for calculating the global mean. The grid is a single
# zone covering the range of the input grid
def createGlobalMeanGrid(grid):
    inlat = grid.getLatitude()
    inlatBounds, inlonBounds = grid.getBounds()
    outlatArray = numpy.array([(inlat[0] + inlat[-1])/2.0])
    outlatBounds = numpy.array([[inlatBounds[0,0], inlatBounds[-1,1]]])
    outlat = createAxis(outlatArray, outlatBounds)
    outlat.units = inlat.units

    inlon = grid.getLongitude()
    outlonArray = numpy.array([(inlon[0] + inlon[-1])/2.0])
    outlonBounds = numpy.array([[inlonBounds[0,0], inlonBounds[-1,1]]])
    outlon = createAxis(outlonArray, outlonBounds)
    outlon.units = inlon.units

    return createRectGrid(outlat,outlon,grid.getOrder())

# Generate a grid for zonal averaging. The grid has the same latitudes
# as the input grid, and a single longitude.
def createZonalGrid(grid):
    inlat = grid.getLatitude()
    outlatBounds, inlonBounds = grid.getBounds()
    outlat = createAxis(inlat[:],outlatBounds)
    outlat.units = inlat.units

    inlon = grid.getLongitude()
    outlonArray = numpy.array([(inlon[0] + inlon[-1])/2.0])
    outlonBounds = numpy.array([[inlonBounds[0,0], inlonBounds[-1,1]]])
    outlon = createAxis(outlonArray, outlonBounds)
    outlon.units = inlon.units

    return createRectGrid(outlat,outlon,grid.getOrder())

# Generate a generic (untyped) grid from lat, lon vectors
def createGenericGrid(latArray, lonArray, latBounds=None, lonBounds=None, order="yx", mask=None):
    lat = createAxis(latArray,latBounds)
    lat.units = "degrees_north"
    lon = createAxis(lonArray,lonBounds)
    lon.units = "degrees_east"
    return createRectGrid(lat,lon,order,"generic",mask)

def createGaussianGrid(nlats, xorigin=0.0, order="yx"):
    """ createGaussianGrid(nlats, xorigin=0.0)
    Create a Gaussian grid, with shape (nlats, 2*nlats).
    'nlats' is the number of latitudes.
    'xorigin' is the origin of the longitude axis.
    'order' is either "yx" or "xy" """
    lat = createGaussianAxis(nlats)
    nlons = 2*nlats
    lon = createUniformLongitudeAxis(xorigin, nlons, 360.0/float(nlons))
    return createRectGrid(lat, lon, order, "gaussian")

# Functions for coordinate region specifications
LongitudeType = 'lon'
LatitudeType = 'lat'
VerticalType = 'lev'
TimeType = 'time'
CoordinateTypes = [LongitudeType, LatitudeType, VerticalType, TimeType]

# Note: no time dimensions in grids.
CoordTypeToLoc = {LongitudeType:0, LatitudeType:1, VerticalType:2}

def defaultRegion():
    """Return a specification for a default (full) region."""
    return [None]*3

def setRegionSpecs(grid, coordSpec, coordType, resultSpec):
    """Modify a list of coordinate specifications, given a coordinate type and
    a specification for that coordinate.
    'grid' is the grid object to be associated with the region.
    'coordSpec' is a coordinate specification, having one of the forms:

        x
        (x,y)
        (x,y,'co')
        (x,y,'co',cycle)
        ':'
        None

    'coordType' is one of CoordinateTypes
    'resultSpec' is a list of 4-tuples of the form (x,y,'co',cycle), or None
      if no spec for the corresponding dimension type.

    The function sets the appropriate coordinate in resultSpec,
    in the canonical form (x,y,'co',cycle). A CDMSError exception
    is raised if the entry in resultSpec is not None.

    Note that time coordinate types are not permitted.
    """
    
    if (coordSpec is None) or (coordSpec==':'):
        canonSpec = None
    elif type(coordSpec) is types.TupleType:
        if len(coordSpec)==2:
            canonSpec = (coordSpec[0],coordSpec[1],'cc',None)
        elif len(coordSpec)==3:
            canonSpec = (coordSpec[0],coordSpec[1],coordSpec[2],None)
        elif len(coordSpec)!=4:
            raise CDMSError, 'Invalid coordinate specification: %s'%`coordSpec`
    elif type(coordSpec) in [types.IntType, types.FloatType]:
        canonSpec = (coordSpec, coordSpec, 'cc', None)
    else:
        raise CDMSError, 'Invalid coordinate specification: %s'%`coordSpec`

    coordLoc = CoordTypeToLoc[coordType]
    if coordLoc is None:
        raise CDMSError, 'Invalid coordinate type: %s'%coordType

    if resultSpec[coordLoc] is not None:
        raise CDMSError, 'Multiple specifications for coordinate type %s'%coordType
    resultSpec[coordLoc] = canonSpec

class AbstractGrid (CdmsObj):

    def __init__ (self, node):
        CdmsObj.__init__ (self, node)
        self.id = '<None>' # String identifier
        if node is not None and hasattr(node,'id'): self.id = node.id
        self.parent = None #Dataset containing this grid
        self._flataxes_ = None
        self._mesh_ = None

    def listall (self, all=None):
        result=[]
        result.append('Grid has Python id %s.' % hex(id(self)))
        return result

    def __str__ (self):
        return string.join(self.listall(), "\n") + "\n"

    __repr__ = __str__

    def info(self, flag=None, device=None):
        "Write info about slab; include dimension values and weights if flag"
        if device is None: device = sys.stdout
        device.write(str(self))

    def writeToFile(self, file):
        """Write self to a CdmsFile file, returning CF coordinates attribute, or None if not applicable"""
        raise CDMSError, MethodNotImplemented

    def subSlice(self, *specs, **keys):
        """Get a subgrid based on an argument list <specs> of slices."""
        raise CDMSError, MethodNotImplemented

    def hasCoordType(self, coordType):
        """Return 1 iff self has the coordinate type."""
        return 0

    def getAxisList(self):
      axes =[]
      for i in range(len(self._order_)):
        axes.append(self.getAxis(i))
      return axes

    def isClose(self, g):
        """Return 1 if g is 'close enough' to self to be considered equal, 0 if not."""
        return 0

    def checkAxes(self, axes):
        """Return 1 iff self.getAxisList and axes are consistent."""
        return 1

    def reconcile(self, axes):
        """Return a grid that is consistent with the axes, or None."""
        return self

    def clone(self, copyData=1):
        """Make a copy of self."""
        raise CDMSError, MethodNotImplemented

    def flatAxes(self):
        """Return (flatlat, flatlon) where flatlat is a raveled NumPy array
        having the same length as the number of cells in the grid, similarly
        for flatlon."""
        raise CDMSError, MethodNotImplemented

    def size(self):
        "Return number of cells in the grid"
        raise CDMSError, MethodNotImplemented

    def writeScrip(self, cdunifFile):
        "Write a grid to a SCRIP file"
        raise CDMSError, MethodNotImplemented

class AbstractRectGrid(AbstractGrid):
    """AbstractRectGrid defines the interface for rectilinear grids:
       grids which can be decomposed into 1-D latitude and longitude axes
    """
    gridtypes = ['gaussian','uniform','equalarea','generic']

    def __init__ (self, node):
        AbstractGrid.__init__ (self, node)
        val = self.__cdms_internals__ + ['id',]
        self.___cdms_internals__ = val

    def listall (self, all=None):
        result = AbstractGrid.listall(self, all)
        result.append('Gridtype: ' + self.getType())
        result.append('Grid shape: ' + str(self.shape))
        result.append('Order: ' + self.getOrder())
        if all:
            result.append('Weights:')
            result.append(str(self.getWeights()))
        return result

    def _getshape (self):
        if self._order_ == "yx":
            return (len(self._lataxis_),len(self._lonaxis_))
        else:
            return (len(self._lonaxis_),len(self._lataxis_))

    # Get the n-th axis. naxis is 0 or 1.
    def getAxis(self, naxis):
        ind = self._order_[naxis]
        if ind=='x':
            axis = self.getLongitude()
        else:
            axis = self.getLatitude()
        return axis

    def getBounds(self):
        latbnds, lonbnds = (self._lataxis_.getExplicitBounds(), self._lonaxis_.getExplicitBounds())
        if (latbnds is None or lonbnds is None) and getAutoBounds() in [1,2]:
            nlatbnds, nlonbnds = self.genBounds()
            if latbnds is None:
                latbnds = nlatbnds
            if lonbnds is None:
                lonbnds = nlonbnds

        return (latbnds, lonbnds)

    def getLatitude(self):
        return self._lataxis_

    def getLongitude(self):
        return self._lonaxis_

    def getMask(self):
        raise CDMSError, MethodNotImplemented

    def setMask(self,mask,permanent=0):
        raise CDMSError, MethodNotImplemented

    def getOrder(self):
        return self._order_

    def getType(self):
        return self._gridtype_

    def setType(self, gridtype):
        if gridtype=='linear': gridtype='uniform'
        if gridtype=='unknown': gridtype='generic'
        # assert gridtype in AbstractRectGrid.gridtypes, 'Grid type must be one of %s'%`AbstractRectGrid.gridtypes`
        self._gridtype_ = gridtype

    # Return normalized area weights, as latWeights, lonWeights:
    #   latWeights[i] = 0.5 * abs(sin(latBnds[i+1]) - sin(latBnds[i]))
    #   lonWeights[i] = abs(lonBnds[i+1] - lonBnds[i])/360.0
    # Assumes that both axes are represented in degrees.
    def getWeights(self):

        latBounds, lonBounds = self.getBounds()
        latBounds = (numpy.pi/180.0) * latBounds
        latWeights = 0.5 * numpy.absolute(numpy.sin(latBounds[:,1]) - numpy.sin(latBounds[:,0]))

        lonWeights = numpy.absolute((lonBounds[:,1] - lonBounds[:,0]))/360.0

        return latWeights, lonWeights

    # Create a transient grid for the index (tuple) intervals.
    def subGrid(self,latinterval, loninterval):
        if latinterval is None: latinterval = (0, len(self._lataxis_))
        if loninterval is None: loninterval = (0, len(self._lonaxis_))
            
        latobj = self._lataxis_.subaxis(latinterval[0],latinterval[1])
        lonobj = self._lonaxis_.subaxis(loninterval[0],loninterval[1])
        maskArray = self.getMask()
        if maskArray is not None:
            if self._order_=="yx":
                submask = maskArray[latinterval[0]:latinterval[1], loninterval[0]:loninterval[1]]
            else:
                submask = maskArray[loninterval[0]:loninterval[1], latinterval[0]:latinterval[1]]
        else:
            submask = None

        return TransientRectGrid(latobj, lonobj, self._order_, self._gridtype_, submask)
        
    # Same as subGrid, for coordinates
    def subGridRegion(self, latRegion, lonRegion):
        latInterval = self._lataxis_.mapInterval(latRegion)
        lonInterval = self._lonaxis_.mapInterval(lonRegion)
        return self.subGrid(latInterval, lonInterval)

    # Return a transient grid which is the transpose of this grid
    def transpose(self):
        if self._order_=="yx":
            neworder = "xy"
        else:
            neworder = "yx"

        maskArray = self.getMask()
        if maskArray is not None:
            newmask = numpy.transpose(maskArray)
        else:
            newmask = None

        return TransientRectGrid(self._lataxis_, self._lonaxis_, neworder, self._gridtype_, newmask)

    # Generate a best guess at grid info for a single grid
    # Return a tuple (type,nlats,isoffset) where:
    #   type == ('gaussian' | 'equalarea' | 'uniform' | 'generic')
    #   nlats is the number of latitudes of the grid:
    #     if gaussian, the gaussian nlats minus pole values
    #     if equalarea, the equalarea nlats minus pole values
    #   isoffset is true iff this is a BOUNDARY grid, hence the bounds
    #     are the points wrt nlat, plus the poles.
    def classify(self):
        import regrid2._regrid

        CLOSE_ENOUGH = 1.e-3
        lat = self.getLatitude()
        if len(lat)==1:
            return ('generic',1,0)

        latar = lat[:]
        if lat[0]<lat[-1]:              # increasing?
            hassouth = (abs(lat[0]+90.0)<1.e-2)
            hasnorth = (abs(lat[-1]-90.0)<1.e-2)
            if hassouth: latar = latar[1:]
            if hasnorth: latar = latar[:-1]
        else:                           # decreasing
            hassouth = (abs(lat[-1]+90.0)<1.e-2)
            hasnorth = (abs(lat[0]-90.0)<1.e-2)
            if hassouth: latar = latar[:-1]
            if hasnorth: latar = latar[1:]
        nlats = len(latar)

        # Get the related Gaussian latitude
        gausslatns, wts, bnds = regrid2._regrid.gridattr(len(latar),'gaussian')
        gausslatsn = gausslatns[::-1]
        diffs = latar[1:]-latar[:-1]
        equalareans, wts, bnds = regrid2._regrid.gridattr(len(latar),'equalarea')
        equalareasn = equalareans[::-1]

        # Get the Gaussian lats for len+1, in case this is a boundary
        dumlat, dumwt, bndsplusns = regrid2._regrid.gridattr(len(latar)+1,'gaussian')
        bndsplussn = bndsplusns[::-1]

        # Look for N-S equality
        isoffset = 0
        if numpy.alltrue(numpy.less(numpy.absolute(latar[:]-gausslatns),CLOSE_ENOUGH)):
            actualType = 'gaussian'

        elif numpy.alltrue(numpy.less(numpy.absolute(latar[:]-gausslatsn),CLOSE_ENOUGH)):
            actualType = 'gaussian'

        # Check for zone (offset) variable
        elif numpy.alltrue(numpy.less(numpy.absolute(latar[:]-bndsplusns[1:-1]),CLOSE_ENOUGH)):
            actualType = 'gaussian'
            isoffset = 1
            nlats = nlats+1

        elif numpy.alltrue(numpy.less(numpy.absolute(latar[:]-bndsplussn[1:-1]),CLOSE_ENOUGH)):
            actualType = 'gaussian'
            isoffset = 1
            nlats = nlats+1

        elif numpy.alltrue(numpy.less(numpy.absolute(diffs-diffs[0]),CLOSE_ENOUGH)):
            actualType = 'uniform'

        elif numpy.alltrue(numpy.less(numpy.absolute(latar[:]-equalareans),CLOSE_ENOUGH)):
            actualType = 'equalarea'

        elif numpy.alltrue(numpy.less(numpy.absolute(latar[:]-equalareasn),CLOSE_ENOUGH)):
            actualType = 'equalarea'

        else:
            actualType = 'generic'

        return (actualType,nlats,isoffset)

    # Generate a best guess at grid info within a family of grids (list of grids)
    # Return a tuple (type,coverage,nlats,isoffset, basegrid, latindex) where:
    #   type == ('gaussian' | 'equalarea' | 'uniform' | 'generic')
    #   coverage == ('global' | 'regional')
    #   nlats is the number of latitudes of the FULL grid:
    #     if gaussian, the gaussian nlats minus pole values
    #     if equalarea, the equalarea nlats minus pole values
    #     if regional, nlats for the full grid, of which this is a subset
    #     if offset, nlats for which this is the BOUNDARY grid
    #   isoffset is true iff this is a BOUNDARY grid, hence the bounds
    #     are the points wrt nlat, plus the poles.
    #   basegrid is the full grid, if this is regional, or None
    #   latindex is index into basegrid latitude, or None
    def classifyInFamily(self, gridlist):
        gridtype, nlats, isoffset = self.classify()
        coverage = 'global'
        basegrid = None
        latindex = None
        if gridtype=='generic':
            # Look for truncated grids: such that grid is a subset of grid2
            found = 0
            for grid2 in gridlist:
                if self.id==grid2.id: continue
                lat = self.getLatitude()
                lon = self.getLongitude()
                lat2 = grid2.getLatitude()
                lon2 = grid2.getLongitude()
                if len(lat)>len(lat2) or len(lon)>len(lon2): continue
                latIsSubset, latindex = isSubsetVector(lat[:],lat2[:],1.e-2)
                lonIsSubset, lonindex = isSubsetVector(lon[:],lon2[:],1.e-2)
                if latIsSubset and lonIsSubset:
                    found = 1
                    if len(lat2)>nlats:
                        coverage = 'regional'
                    nlats = len(lat2)
                    basegrid = grid2.id
                    break

        return (gridtype, coverage, nlats, isoffset, basegrid, latindex)

    # Generate default bounds
    def genBounds(self):
        import regrid2._regrid

        if hasattr(self,"parent") and self.parent is not None:
            gridfamily = self.parent.grids.values()
        else:
            gridfamily = []

        gridtype, coverage, nlats, isoffset, basegrid, latindex = self.classifyInFamily(gridfamily)
        if _classifyGrids==0:
            gridtypenew = self.getType()
            if gridtypenew in AbstractRectGrid.gridtypes:
                gridtype = gridtypenew

        # Get latitude bounds
        lat = self.getLatitude()
        ascending = (lat[0] < lat[-1])
        if gridtype=='gaussian':
            pts, wts, bnds = regrid2._regrid.gridattr(nlats, 'gaussian')
            if ascending: bnds = bnds[::-1]
            latbnds = numpy.zeros((len(lat),2),numpy.float)
            latbnds[:,0] = bnds[:-1]
            latbnds[:,1] = bnds[1:]
            latbnds[0,:] = numpy.maximum(-90.0, numpy.minimum(90.0,latbnds[0,:]))
            latbnds[-1,:] = numpy.maximum(-90.0, numpy.minimum(90.0,latbnds[-1,:]))
        elif gridtype=='equalarea':
            pts, wts, bnds = regrid2._regrid.gridattr(nlats, 'equalarea')
            if ascending: bnds = bnds[::-1]
            latbnds = numpy.zeros((len(lat),2),numpy.float)
            latbnds[:,0] = bnds[:-1]
            latbnds[:,1] = bnds[1:]
            latbnds[0,:] = numpy.maximum(-90.0, numpy.minimum(90.0,latbnds[0,:]))
            latbnds[-1,:] = numpy.maximum(-90.0, numpy.minimum(90.0,latbnds[-1,:]))
        else:
            latbnds = lat.genGenericBounds()

        # Stretch latitude bounds to +/- 90.0
        if ascending:
            latbnds[0,0] = min(latbnds[0,0],-90.0)
            latbnds[-1,1] = max(latbnds[-1,1],90.0)
        else:
            latbnds[0,0] = max(latbnds[0,0],+90.0)
            latbnds[-1,1] = min(latbnds[-1,1],-90.0)

        # Get longitude bounds
        lon = self.getLongitude()
        if len(lon)>1:
            lonbnds = lon.genGenericBounds()
        else:
            lonbnds = numpy.array([[lon[0]-180.0, lon[0]+180.0]],numpy.float)

        return (latbnds, lonbnds)

    def writeToFile(self, file):
        return None

    def getMesh(self):
        """Generate a mesh array for the meshfill graphics method."""
        if self._mesh_ is None:
            LAT=0
            LON=1
            latbounds, lonbounds = self.getBounds()
            if latbounds is None or lonbounds is None:
                raise CDMSError, 'No boundary data is available for grid %s'%self.id
            ny = len(self._lataxis_)
            nx = len(self._lonaxis_)
            lenmesh = ny*nx
            mesh = numpy.zeros((lenmesh,2,4),latbounds.dtype.char)
            broadlat = numpy.repeat(latbounds[:,numpy.newaxis,:],nx,axis=1)
            broadlat.shape = (lenmesh,2)
            broadlon = numpy.repeat(lonbounds[numpy.newaxis,:,:],ny,axis=0)
            broadlon.shape=(lenmesh,2)
            mesh[:,LAT,0] = broadlat[:,0]
            mesh[:,LAT,1] = broadlat[:,0]
            mesh[:,LAT,2] = broadlat[:,1]
            mesh[:,LAT,3] = broadlat[:,1]
            mesh[:,LON,0] = broadlon[:,0]
            mesh[:,LON,1] = broadlon[:,1]
            mesh[:,LON,2] = broadlon[:,1]
            mesh[:,LON,3] = broadlon[:,0]
            self._mesh_ = mesh
        return self._mesh_

    def flatAxes(self):
        """Return (flatlat, flatlon) where flatlat is a 1D NumPy array
        having the same length as the number of cells in the grid, similarly
        for flatlon."""

        if self._flataxes_ is None:
            alat = self.getLatitude()[:]
            alon = self.getLongitude()[:]
            alatflat = numpy.repeat(alat[:,numpy.newaxis], len(alon), axis=1)
            alonflat = numpy.repeat(alon[numpy.newaxis,:], len(alat), axis=0)
            self._flataxes_ = (numpy.ravel(alatflat), numpy.ravel(alonflat))
        return self._flataxes_

    def size(self):
        ny, nx = self.shape
        return ny*nx

    def writeScrip(self, cufile, gridTitle=None):
        """Write a grid to a SCRIP file.
        cufile is a Cdunif file, NOT a CDMS file.
        gridtitle is a string identifying the grid.
        """
        cgrid = self.toCurveGrid()
        cgrid.writeScrip(cufile, gridTitle)

    def toCurveGrid(self, gridid=None):
        """Convert to a curvilinear grid.
        'gridid' is the string identifier of the resulting curvilinear grid object.
        """

        from coord import TransientVirtualAxis, TransientAxis2D
        from hgrid import TransientCurveGrid

        lat = self._lataxis_[:]
        lon = self._lonaxis_[:]

        latunits = ''
        if hasattr(self._lataxis_, 'units'):
            latunits = self._lataxis_.units

        lonunits = ''
        if hasattr(self._lonaxis_, 'units'):
            lonunits = self._lonaxis_.units

        blat, blon = self.getBounds()
        mask = self.getMask()

        nlat = len(lat)
        nlon = len(lon)

        order = self.getOrder()

        # Deal with the order of the axes
        # ax - first index, ay - second index
        if re.search(order, 'xy', re.I):
            orderXY = True
            ax, ay = lat, lon
            bx, by = blat, blon
            nx, ny = nlat, nlon
        else:
            orderXY = False
            ax, ay = lon, lat
            bx, by = blon, blat
            nx, ny = nlon, nlat
            
        centerX = numpy.outer(numpy.ones(ny), ax)
        centerY = numpy.outer(ay, numpy.ones(nx))

        # Create corner latitudes (in yx order), ensuring counterclockwise direction
        cy = numpy.zeros((ny, 4), numpy.float)
        if (by[0,0]<= by[0,1]):
            incr = 1
        else:
            incr = 0
        cy[:,0] = by[:,1-incr]
        cy[:,1] = by[:,1-incr]
        cy[:,2] = by[:,incr]
        cy[:,3] = by[:,incr]
        cornerY = numpy.repeat(cy[:,numpy.newaxis,:], nx, axis=1)
        
        # Create corner longitudes (in yx order), ensuring counterclockwise direction
        cx = numpy.zeros((nx, 4), numpy.float)
        if (bx[0,0]<= bx[0,1]):
            incr = 1
        else:
            incr = 0
        cx[:,0] = bx[:,1-incr]
        cx[:,1] = bx[:,incr]
        cx[:,2] = bx[:,incr]
        cx[:,3] = bx[:,1-incr]
        cornerX = numpy.repeat(cx[numpy.newaxis,:,:], ny, axis=0)

        iaxis = TransientVirtualAxis("i",ny) # First axis
        jaxis = TransientVirtualAxis("j",nx) # Second axis
        
        centerLat = centerY
        centerLon = centerX
        cornerLat = cornerY
        cornerLon = cornerX
        if orderXY:
            centerLat = centerX
            centerLon = centerY
            cornerLat = cornerX
            cornerLon = cornerY
            

        lataxis = TransientAxis2D(centerLat, axes=(iaxis, jaxis), bounds=cornerLat,
                                  attributes={'units':latunits}, id="latitude")
        lonaxis = TransientAxis2D(centerLon, axes=(iaxis, jaxis), bounds=cornerLon,
                                  attributes={'units':lonunits}, id="longitude")
        grid = TransientCurveGrid(lataxis, lonaxis, id=gridid, tempmask=mask)

        return grid

    def toGenericGrid(self, gridid=None):
        curvegrid = self.toCurveGrid()
        gengrid = curvegrid.toGenericGrid(gridid=gridid)
        return gengrid

    shape = property(_getshape,None)
   
## PropertiedClasses.set_property (AbstractRectGrid, 'shape', 
##                                 AbstractRectGrid._getshape, 
##                                 nowrite=1,
##                                 nodelete=1)

## internattr.add_internal_attribute (AbstractRectGrid, 'id', 'parent')

class RectGrid(AbstractRectGrid):

    def __init__(self,parent,rectgridNode=None):
        if rectgridNode is not None and rectgridNode.tag != 'rectGrid':
            raise CDMSError, 'Node is not a grid node'
        AbstractRectGrid.__init__(self,rectgridNode)
        self.parent = parent

    # Set pointers to related structural elements: lon, lat axes, order, mask
    def initDomain(self, axisdict, vardict):
        if not axisdict.has_key(self.latitude):
            raise CDMSError, 'No such latitude: %s'%`self.latitude`
        if not axisdict.has_key(self.longitude):
            raise CDMSError, 'No such longitude: %s'%`self.longitude`
        self._lataxis_ = axisdict[self.latitude]
        self._lonaxis_ = axisdict[self.longitude]
        self._order_ = self.order
        self._gridtype_ = self.attributes.get('type')
        if self._gridtype_ is None: self._gridtype_ = "generic"
        if hasattr(self,"mask"):
            self._maskVar_ = vardict.get(self.mask)
        else:
            self._maskVar_ = None
    
    def getMask(self):
        if self._maskVar_ is None:
            # return numpy.ones(self.shape)
            return None
        else:
            return self._maskVar_[:]

    def getMaskVar(self):
        return self._maskVar_

## internattr.add_internal_attribute(RectGrid)

class FileRectGrid(AbstractRectGrid):

    def __init__(self, parent, gridname, latobj, lonobj, order, gridtype, maskobj=None, tempMask=None):
        AbstractRectGrid.__init__(self, None)
        self.id = gridname
        self.parent = parent
        self._lataxis_ = latobj
        self._lonaxis_ = lonobj
        if not order in ["yx","xy"]:
            raise CDMSError, 'Grid order must be "yx" or "xy"'
        self._order_ = order
        self.setType(gridtype)
        self._maskVar_ = maskobj        # FileVariable of mask
        self.setMask(tempMask)    # numpy array, which overrides the permanent mask

    # Set bounds. If persistent==1, write to file, else just shadow any file boundaries.
    def setBounds(self, latBounds, lonBounds, persistent=0):
        self._lataxis_.setBounds(latBounds, persistent)
        self._lonaxis_.setBounds(lonBounds, persistent)

    # Return the mask array (NOT the mask variable).
    def getMask(self):
        if self._tempMask_ is not None:
            return self._tempMask_
        elif self._maskVar_ is None:
            # return numpy.ones(self.shape)
            return None
        else:
            return self._maskVar_[:]

    # Set the mask to array 'mask'. If persistent == 1, modify permanently
    # in the file, else set as a temporary mask.
    def setMask(self,mask,persistent=0):
        if persistent!=0: raise CDMSError, MethodNotImplemented
        if mask is None:
            self._tempMask_ = None
        else:
            assert type(mask)==numpy.ndarray, 'Mask must be a numpy array'
            assert mask.shape==self.shape,'Mask must have shape %s'%`self.shape`
            self._tempMask_ = copy.copy(mask)

    def getMaskVar(self):
        return self._maskVar_

## internattr.add_internal_attribute(FileRectGrid)

# In-memory rectilinear grid
class TransientRectGrid(AbstractRectGrid):
    "Grids that live in memory only."
    def __init__(self, latobj, lonobj, order, gridtype, maskarray=None):
        AbstractRectGrid.__init__(self,None)
        if latobj.__class__ != TransientAxis:
            latobj = TransientAxis(latobj[:], latobj.getBounds())
        if lonobj.__class__ != TransientAxis:
            lonobj = TransientAxis(lonobj[:], lonobj.getBounds())
        self._lataxis_ = latobj
        self._lataxis_.designateLatitude()
        self._lonaxis_ = lonobj
        self._lonaxis_.designateLongitude()
        if not order in ["yx","xy"]:
            raise CDMSError, 'Grid order must be "yx" or "xy"'
        self._order_ = order
        self.setType(gridtype)
        self.setMask(maskarray)        # numpy mask array

    def getMask(self):
        if self._maskArray_ is None:
            # return numpy.ones(self.shape)
            return None
        else:
            return self._maskArray_

    # Set the mask. The persistent argument is provided for compatibility
    # with persistent versions, is ignored.
    def setMask(self,mask, persistent=0):
        if mask is not None:
            if type(mask)!=numpy.ndarray:
               raise CDMSError, 'Mask must be a numpy array'
            if mask.shape != self.shape:
               raise CDMSError, 'Mask must have shape %s'%`self.shape`
        self._maskArray_ = copy.copy(mask)

    def setBounds(self, latBounds, lonBounds):
        self._lataxis_.setBounds(latBounds)
        self._lonaxis_.setBounds(lonBounds)

## internattr.add_internal_attribute(TransientRectGrid)

def isGrid(grid):
    """
    Is grid a grid?
    @param grid cdms2 contruct to be examined
    """
    return isinstance(grid, AbstractGrid)


def writeScripGrid(path, grid, gridTitle=None):
    """Write a grid to a SCRIP grid file.
    path is the path of the SCRIP file to be created.
    grid is a CDMS grid object.
    gridTitle is a string ID for the grid.
    """
    
    import Cdunif
    f = Cdunif.CdunifFile(path,'w')
    grid.writeScrip(f, gridTitle)
    f.close()

