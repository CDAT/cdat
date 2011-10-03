## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"""CDMS Generic Grids"""

import numpy
## import PropertiedClasses
import bindex
from error import CDMSError
from grid import LongitudeType, LatitudeType, VerticalType, TimeType, CoordTypeToLoc
from hgrid import AbstractHorizontalGrid
from axis import allclose

MethodNotImplemented = "Method not yet implemented"

class AbstractGenericGrid(AbstractHorizontalGrid):

    def __init__(self, latAxis, lonAxis, id=None, maskvar=None, tempmask=None, node=None):
        """Create a generic grid.
        """
        if latAxis.shape != lonAxis.shape:
            raise CDMSError, 'Latitude and longitude axes must have the same shape.'
        AbstractHorizontalGrid.__init__(self, latAxis, lonAxis, id, maskvar, tempmask, node)
        self._index_ = None

    def clone(self, copyData=1):
        newlat = self._lataxis_.clone(copyData)
        newlon = self._lonaxis_.clone(copyData)
        return TransientGenericGrid(newlat, newlon, id=self.id)

#    def __repr__(self):
#        return "<GenericGrid, id: %s, shape: %s>"%(self.id, `self.shape`)
#    __str__ = __repr__

    def getMesh(self, transpose=None):
        """Generate a mesh array for the meshfill graphics method.
        'transpose' is for compatibility with other grid types, is ignored."""
        import MV2 as MV
        if self._mesh_ is None:
            LAT=0
            LON=1
            latbounds, lonbounds = self.getBounds()
            if latbounds is None or lonbounds is None:
                raise CDMSError, 'No boundary data is available for grid %s'%self.id
            nvert = latbounds.shape[-1]
            mesh = numpy.zeros((self.size(),2,nvert),latbounds.dtype.char)
            mesh[:,LAT,:] = MV.filled(latbounds)
            mesh[:,LON,:] = MV.filled(lonbounds)
            self._mesh_ = mesh
        return self._mesh_

    def _getShape (self):
        return self._lataxis_.shape

    # Get the n-th index axis. naxis is 0 or 1.
    def getAxis(self, naxis):
        return self._lataxis_.getAxis(naxis)

    # Don't try to generate bounds for generic grids
    def genBounds(self):
        return (None, None)

    def getMask(self):
        """Get the mask array, if any, otherwise None is returned."""
        if self._maskVar_ is None:
            return None
        else:
            return self._maskVar_[:]

    def size(self):
        return self._lataxis_.size()

    def writeScrip(self, cufile, gridTitle=None):
        """Write a grid to a SCRIP file.
        cufile is a Cdunif file, NOT a CDMS file.
        gridtitle is a string identifying the grid.
        """

        lat = numpy.ma.filled(self._lataxis_)
        lon = numpy.ma.filled(self._lonaxis_)
        blat, blon = self.getBounds()
        ngrid, ncorners = blat.shape
        mask = self.getMask()
        if mask is None:
            mask = numpy.ones((ngrid,), numpy.int32)
        else:
            mask[:] = 1 - mask
            mask = mask.astype(numpy.int32)

        # Write the file
        if gridTitle is None:
            gridTitle = self.id
        cufile.title = gridTitle
        cufile.createDimension("grid_size", ngrid)
        cufile.createDimension("grid_corners", ncorners)
        cufile.createDimension("grid_rank", 1)
        griddims = cufile.createVariable("grid_dims", numpy.int, ("grid_rank",))
        gridcenterlat = cufile.createVariable("grid_center_lat", numpy.float, ("grid_size",))
        gridcenterlat.units = "degrees"
        gridcenterlon = cufile.createVariable("grid_center_lon", numpy.float, ("grid_size",))
        gridcenterlon.units = "degrees"
        gridimask = cufile.createVariable("grid_imask", numpy.int, ("grid_size",))
        gridimask.units = "unitless"
        gridcornerlat = cufile.createVariable("grid_corner_lat", numpy.float, ("grid_size","grid_corners"))
        gridcornerlat.units = "degrees"
        gridcornerlon = cufile.createVariable("grid_corner_lon", numpy.float, ("grid_size","grid_corners"))
        gridcornerlon.units = "degrees"

        griddims[:] = numpy.array([ngrid], numpy.int32)
        gridcenterlat[:] = lat
        gridcenterlon[:] = lon
        gridimask[:] = mask
        gridcornerlat[:] = numpy.ma.filled(blat)
        gridcornerlon[:] = numpy.ma.filled(blon)

    def writeToFile(self, file):
        latvar = self._lataxis_.writeToFile(file)
        lonvar = self._lonaxis_.writeToFile(file)
        if self._maskVar_ is not None:
            maskid = "mask_"+self.id
            file.write(self._maskVar_, id=maskid)
            latvar.maskid = maskid
            lonvar.maskid = maskid
        return (latvar, lonvar)

    def subSlice(self, *specs, **keys):
        """Get a transient subgrid based on an argument list <specs> of slices."""

        newlat = self._lataxis_.subSlice(*specs, **keys)
        newlon = self._lonaxis_.subSlice(*specs, **keys)
        if self._maskVar_ is None:
            newmask = None
        else:
            newmask = self._maskVar_.subSlice(*specs, **keys)

        result = TransientGenericGrid(newlat, newlon, maskvar=newmask)
        return result

    def getGridSlices(self, domainlist, newaxislist, slicelist):
        """Determine which slices in slicelist correspond to the lat/lon elements
        of the grid.
        domainlist is a list of axes of a variable.
        newaxislist is a list of result axes after the slicelist is applied to domainlist.
        slicelist is a list of slices.

        All lists are of equal length.

        Return value is (newslicelist, gridaxislist) where
        newslicelist is the elements of slicelist that correspond to the grid, in the
          preferred order of the grid.
        gridaxislist is the elements of newaxislist that correspond to the grid, in the
          preferred order of the grid.
        """
        
        iaxis = self._lataxis_.getAxis(0)
        k = 0
        i = -1
        for d in domainlist:
            if d is iaxis:
                inewaxis = newaxislist[k]
                islice = slicelist[k]
                i = k
            k += 1

        if i==-1:
            raise RuntimeError, 'Grid lat/lon domains do not match variable domain'

        return ((islice, ), (inewaxis, ))

    def getIndex(self):
        """Get the grid index"""
        if self._index_ is None:
            latlin = numpy.ma.filled(self._lataxis_)
            lonlin = numpy.ma.filled(self._lonaxis_)
            self._index_ = bindex.bindexHorizontalGrid(latlin, lonlin)

        return self._index_

    def intersect(self, spec):
        """Intersect with the region specification.

        'spec' is a region specification of the form defined in the grid module.

        Returns (mask, indexspecs) where
        'mask' is the mask of the result grid AFTER self and region spec are interested.
        'indexspecs' is a dictionary of index specifications suitable for slicing a
          variable with the given grid.
        """

        ncell = self.shape
        index = self.getIndex()
        latspec = spec[CoordTypeToLoc[LatitudeType]]
        lonspec = spec[CoordTypeToLoc[LongitudeType]]
        latlin = numpy.ma.filled(self._lataxis_)
        lonlin = numpy.ma.filled(self._lonaxis_)
        lonlin = numpy.ma.where(numpy.ma.greater_equal(lonlin,360.0), lonlin-360.0, lonlin)
        points = bindex.intersectHorizontalGrid(latspec, lonspec, latlin, lonlin, index)
        if len(points)==0:
            raise CDMSError, 'No data in the specified region, longitude=%s, latitude=%s'%(`lonspec`, `latspec`)

        fullmask = numpy.ones(ncell)
        numpy.put(fullmask, points, 0)
        
        imin, imax  = (min(points), max(points)+1)
        submask = fullmask[imin:imax]

        cellid = self.getAxis(0).id
        indexspecs = {cellid:slice(imin,imax)}

        return submask, indexspecs

    def getAxisList(self):
        return [self._lataxis_.getAxis(0), ]

    def isClose(self, g):
        """Return 1 iff g is a grid of the same type and shape. A real element-by-element
        comparison would be too expensive here."""
        if g is None:
            return 0
        elif self.shape != g.shape:
            return 0
        elif not isinstance(g, AbstractGenericGrid):
            return 0
        else:
            return 1

    def checkAxes(self, axes):
        """Return 1 iff every element of self.getAxisList() is in the list 'axes'."""
        for item in self.getAxisList():
            if item not in axes:
                result = 0
                break
        else:
            result = 1

        return result

    def reconcile(self, axes):
        """Return a grid that is consistent with the axes, or None.
        For curvilinear grids this means that the grid-related axes are
        contained in the 'axes' list. 
        """
        result = self
        selfaxes = self.getAxisList()
        missing = []
        for i in range(1):
            if selfaxes[i] not in axes:
                missing.append(i)
                result = None
            
        # Some of the grid axes are not in the 'axes' list
        if result is None:
            result = self.clone()
            for i in missing:
                for item in axes:
                    if (len(selfaxes[i])==len(item)) and allclose(selfaxes[i], item):
                        result._lataxis_.setAxis(i,item)
                        result._lonaxis_.setAxis(i,item)
                        break
                else:
                    result = None
                    break

        return result

    def flatAxes(self):
        """Return (flatlat, flatlon) where flatlat is a 1D NumPy array
        having the same length as the number of cells in the grid, similarly
        for flatlon."""
        if self._flataxes_ is None:
            import MV2 as MV
            alat = MV.filled(self.getLatitude())
            alon = MV.filled(self.getLongitude())
            self._flataxes_ = (alat, alon)
        return self._flataxes_

    def toGenericGrid(self, gridid=None):
        if gridid is None:
            gridid = self.id
        result = self.clone()
        result.id = gridid
        return result
    shape = property(_getShape,None)
    
## PropertiedClasses.set_property (AbstractGenericGrid, 'shape', 
##                                   AbstractGenericGrid._getShape, nowrite=1,
##                                   nodelete=1)

class DatasetGenericGrid(AbstractGenericGrid):

    def __init__(self, latAxis, lonAxis, id, parent=None, maskvar=None, tempmask=None, node=None):
        """Create a file curvilinear grid.
        """
        AbstractGenericGrid.__init__(self, latAxis, lonAxis, id, maskvar, tempmask, node)
        self.parent = parent

    def __repr__(self):
        return "<DatasetGenericGrid, id: %s, shape: %s>"%(self.id, `self.shape`)

class FileGenericGrid(AbstractGenericGrid):

    def __init__(self, latAxis, lonAxis, id, parent=None, maskvar=None, tempmask=None, node=None):
        """Create a file curvilinear grid.
        """
        AbstractGenericGrid.__init__(self, latAxis, lonAxis, id, maskvar, tempmask, node)
        self.parent = parent

    def __repr__(self):
        return "<FileGenericGrid, id: %s, shape: %s>"%(self.id, `self.shape`)

class TransientGenericGrid(AbstractGenericGrid):

    grid_count = 0

    def __init__(self, latAxis, lonAxis, id=None, maskvar=None, tempmask=None):
        """Create a file curvilinear grid.
        """
        if id is None:
            TransientGenericGrid.grid_count += 1
            id = 'grid_' + str(TransientGenericGrid.grid_count)
        AbstractGenericGrid.__init__(self, latAxis, lonAxis, id, maskvar, tempmask)

    def __repr__(self):
        return "<TransientGenericGrid, id: %s, shape: %s>"%(self.id, `self.shape`)

    def toGenericGrid(self, gridid=None):
        if gridid is None:
            result = self
        else:
            result = self.clone()
            result.id = gridid
        return result

def readScripGenericGrid(fileobj, dims, whichType, whichGrid):
    """Read a 'native' SCRIP grid file, returning a transient generic grid.
    fileobj is an open CDMS dataset or file object.
    dims is the grid shape.
    whichType is the type of file, either "grid" or "mapping"
    if whichType is "mapping", whichGrid is the choice of grid, either "source" or "destination"
    """
    import string
    from auxcoord import TransientAuxAxis1D
    from coord import TransientVirtualAxis

    convention = 'SCRIP'
    if 'S' in fileobj.variables.keys():
        convention = 'NCAR'
        if whichType=="grid":
            gridCornerLatName = 'grid_corner_lat'
            gridCornerLonName = 'grid_corner_lon'
            gridMaskName = 'grid_imask'
            gridCenterLatName = 'grid_center_lat'
            gridCenterLonName = 'grid_center_lon'
            titleName = 'title'
        elif whichGrid=="destination":
            gridCornerLatName = 'yv_b'
            gridCornerLonName = 'xv_b'
            gridMaskName = 'mask_b'
            gridCenterLatName = 'yc_b'
            gridCenterLonName = 'xc_b'
            titleName = 'dest_grid'
        else:
            gridCornerLatName = 'yv_a'
            gridCornerLonName = 'xv_a'
            gridMaskName = 'mask_a'
            gridCenterLatName = 'yc_a'
            gridCenterLonName = 'xc_a'
            titleName = 'source_grid'
    else:
        if whichType=="grid":
            gridCornerLatName = 'grid_corner_lat'
            gridCornerLonName = 'grid_corner_lon'
            gridMaskName = 'grid_imask'
            gridCenterLatName = 'grid_center_lat'
            gridCenterLonName = 'grid_center_lon'
            titleName = 'title'
        elif whichGrid=="destination":
            gridCornerLatName = 'dst_grid_corner_lat'
            gridCornerLonName = 'dst_grid_corner_lon'
            gridMaskName = 'dst_grid_imask'
            gridCenterLatName = 'dst_grid_center_lat'
            gridCenterLonName = 'dst_grid_center_lon'
            titleName = 'dest_grid'
        else:
            gridCornerLatName = 'src_grid_corner_lat'
            gridCornerLonName = 'src_grid_corner_lon'
            gridMaskName = 'src_grid_imask'
            gridCenterLatName = 'src_grid_center_lat'
            gridCenterLonName = 'src_grid_center_lon'
            titleName = 'source_grid'
            
    vardict = fileobj.variables
    cornerLat = fileobj(gridCornerLatName)
    cornerLon = fileobj(gridCornerLonName)
    ncorners = cornerLat.shape[-1]

    if convention == 'NCAR':
        ni = cornerLat.shape[0]
    else:
        ni = dims[0]

    boundsshape = (ni, ncorners)
    if hasattr(cornerLat, 'units') and string.lower(cornerLat.units)[0:6]=='radian':
        cornerLat = (cornerLat*(180.0/numpy.pi)).reshape(boundsshape)
        cornerLon = (cornerLon*(180.0/numpy.pi)).reshape(boundsshape)

    iaxis = TransientVirtualAxis("i",ni)

    if vardict.has_key(gridMaskName):
        # SCRIP convention: 0 for invalid data
        # numpy.ma convention: 1 for invalid data
        mask = 1 - fileobj(gridMaskName)
    else:
        mask = None
        
    if vardict.has_key(gridCenterLatName):
        centerLat = fileobj(gridCenterLatName)
        if hasattr(centerLat, "units") and string.lower(centerLat.units)=='radians':
            centerLat *= (180.0/numpy.pi)
    else:
        centerLat = cornerLat[:,:,0]

    if vardict.has_key(gridCenterLonName):
        centerLon = fileobj(gridCenterLonName)
        if hasattr(centerLon, "units") and string.lower(centerLon.units)=='radians':
            centerLon *= (180.0/numpy.pi)
    else:
        centerLon = cornerLon[:,:,0]

    if hasattr(fileobj,titleName):
        gridid = getattr(fileobj, titleName)
        gridid = string.replace(string.strip(gridid), ' ','_')
    else:
        gridid="<None>"

    lataxis = TransientAuxAxis1D(centerLat, axes=(iaxis,), bounds=cornerLat,
                              attributes={'units':'degrees_north'}, id="latitude")
    lonaxis = TransientAuxAxis1D(centerLon, axes=(iaxis,), bounds=cornerLon,
                              attributes={'units':'degrees_east'}, id="longitude")
    grid = TransientGenericGrid(lataxis, lonaxis, id=gridid, tempmask=mask)

    return grid
