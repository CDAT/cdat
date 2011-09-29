## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"""CDMS HorizontalGrid objects"""

import numpy
import cdms2
import os
import os.path
## import PropertiedClasses
from error import CDMSError
from grid import AbstractGrid, LongitudeType, LatitudeType, VerticalType, TimeType, CoordTypeToLoc
from coord import TransientVirtualAxis
from axis import getAutoBounds, allclose
import bindex

MethodNotImplemented = "Method not yet implemented"

def _flatten(boundsar):
    boundsshape = boundsar.shape
    if len(boundsshape)>2:
        newshape = (reduce((lambda x,y: x*y), boundsshape[:-1], 1), boundsshape[-1])
        boundsar.shape = newshape
    return boundsar

class AbstractHorizontalGrid(AbstractGrid):

    def __init__(self, latAxis, lonAxis, id=None, maskvar=None, tempmask=None, node=None):
        """Create a horizontal grid.
        """
        AbstractGrid.__init__(self, node)
        if id is None:
            self.id = "<None>"
        else:
            self.id = id
        self._lataxis_ = latAxis
        self._lonaxis_ = lonAxis
        self._maskVar_ = maskvar
        self._tempMask_ = tempmask

    # Generate default bounds
    def genBounds(self):
        raise CDMSError, MethodNotImplemented

    # Get the n-th axis. naxis is 0 or 1.
    def getAxis(self, naxis):
        raise CDMSError, MethodNotImplemented

    def getBounds(self):
        """Get the grid cell boundaries, as a tuple (latitudeBounds, longitudeBounds)
        """
        latbnds, lonbnds = (self._lataxis_.getExplicitBounds(), self._lonaxis_.getExplicitBounds())
        if (latbnds is None or lonbnds is None) and getAutoBounds() in [1,2]:
            nlatbnds, nlonbnds = self.genBounds()
            if latbnds is None:
                latbnds = nlatbnds
            if lonbnds is None:
                lonbnds = nlonbnds

        return (latbnds, lonbnds)

    def getLatitude(self):
        """Get the latitude coordinates."""
        return self._lataxis_

    def getLongitude(self):
        """Get the longitude coordinates."""
        return self._lonaxis_

    def getMask(self):
        """Get the mask array, if any, otherwise None is returned."""
        if self._maskVar_ is not None:
            return self._maskVar_
        else:
            return self._tempMask_

    def getMesh(self):
        """Get the mesh array used by the meshfill plot."""
        raise CDMSError, MethodNotImplemented

    def getWeightsArray(self):
        """Return normalized area weights, as an array of the same
        shape as the grid.
        """
        raise CDMSError, MethodNotImplemented

    def listall (self, all=None):
        result=[]
        result.append('Grid has Python id %s.' % hex(id(self)))
        return result

    def setMask(self,mask,permanent=0):
        self._maskVar_ = mask

    def subGridRegion(self, latRegion, lonRegion):
        raise CDMSError, MethodNotImplemented

    def hasCoordType(self, coordType):
        return ((coordType==LatitudeType) or (coordType==LongitudeType))

    def checkConvex(self):
        """Check that each cell of the grid is convex in lon-lat space, with nodes defined counter-clockwise.
        Return a 1D numpy array of cells that fail the cross-product test.
        """

        from numpy import zeros, where, less, logical_or, compress

        latb, lonb = self.getBounds()

        saveshape = lonb.shape
        lonb = _flatten(lonb)
        latb = _flatten(latb)

        ncell, nnode = lonb.shape
        badmask = zeros((ncell,))
        for n0 in range(nnode):
            n1 = (n0+1)%nnode
            n2 = (n1+1)%nnode
            vec0lon = lonb[:,n1] - lonb[:,n0]
            vec0lat = latb[:,n1] - latb[:,n0]
            vec1lon = lonb[:,n2] - lonb[:,n1]
            vec1lat = latb[:,n2] - latb[:,n1]
            cross = vec0lon*vec1lat - vec0lat*vec1lon

            mask = where(less(cross, 0.0), 1, 0)
            badmask = logical_or(mask, badmask)

        badcells = compress(badmask, range(len(badmask)))

        lonb.shape = saveshape
        latb.shape = saveshape

        return badcells

    def fixCutCells(self, nonConvexCells, threshold=270.0):
        """For any mapping from a spherical to a planar surface, there is a linear cut.
        Grid cells that span the cut may appear to be nonconvex, which causes
        problems with meshfill graphics. This routine attempts to 'repair' the cut cell
        boundaries so that meshfill recognizes they are convex.

        nonConvexCells: 1D numpy array of indices of nonconvex cells, as returned from
          checkConvex.
        threshold: positive floating-point value in degrees.
          If the difference in longitude values of
          consecutive boundaries nodes exceeds the threshold, the cell is considered
          a cut cell.

        On return, the grid boundaries are modified.
        Return value is a 1D array of indices of cells that cannot be repaired.
        """

        from numpy import take, array

        latb, lonb = self.getBounds()

        saveshape = lonb.shape
        lonb = _flatten(lonb)
        latb = _flatten(latb)

        ncell, nnode = lonb.shape

        lonb2 = take(lonb, nonConvexCells, axis=0)
        latb2 = take(latb, nonConvexCells, axis=0)

        newbadcells = []
        for k in range(len(nonConvexCells)):
            savelons = lonb2[k]

            # Loop twice
            for node in range(2*nnode):
                n0 = node%nnode
                n1 = (n0+1)%nnode
                vec0lon = lonb2[k,n1]-lonb2[k,n0]
                if vec0lon>threshold:
                    lonb2[k,n1] -= 360.0
                elif vec0lon<-threshold:
                    lonb2[k,n1] += 360.0

            # If the cross-product test still fails, restore
            # the original values and add to the nonConvexCells list
            for n0 in range(nnode):
                n1 = (n0+1)%nnode
                n2 = (n1+1)%nnode
                vec0lon = lonb2[k,n1] - lonb2[k,n0]
                vec0lat = latb2[k,n1] - latb2[k,n0]
                vec1lon = lonb2[k,n2] - lonb2[k,n1]
                vec1lat = latb2[k,n2] - latb2[k,n1]
                cross = vec0lon*vec1lat - vec0lat*vec1lon

                if cross<0:
                    lonb2[k] = savelons
                    newbadcells.append(nonConvexCells[k])
                    break

        # Scatter the repaired cell bounds back to the original bounds
        # and reset the grid bounds.
        for k in range(len(nonConvexCells)):
            lonb[nonConvexCells[k]] = lonb2[k]
        lonb.shape = saveshape
        self.getLongitude().setBounds(lonb)

        return array(newbadcells)

class AbstractCurveGrid(AbstractHorizontalGrid):

    def __init__(self, latAxis, lonAxis, id=None, maskvar=None, tempmask=None, node=None):
        """Create a curvilinear grid.
        """
        if latAxis.shape != lonAxis.shape:
            raise CDMSError, 'Latitude and longitude axes must have the same shape.'
        AbstractHorizontalGrid.__init__(self, latAxis, lonAxis, id, maskvar, tempmask, node)
        self._index_ = None

    def clone(self, copyData=1):
        newlat = self._lataxis_.clone(copyData)
        newlon = self._lonaxis_.clone(copyData)
        return TransientCurveGrid(newlat, newlon, id=self.id)

    def __repr__(self):
        return "<CurveGrid, id: %s, shape: %s>"%(self.id, `self.shape`)
    __str__ = __repr__

    def getMesh(self, transpose=None):
        """Generate a mesh array for the meshfill graphics method.
        If transpose is defined to a tuple, say (1,0), first transpose
        latbounds and lonbounds according to the tuple, (1,0,2) in this case.
        """
        if self._mesh_ is None:
            LAT=0
            LON=1
            latbounds, lonbounds = self.getBounds()
##             ## following work aronud a numpy.ma bug
##             latbounds=latbounds.filled()
##             lonbounds=lonbounds.filled()
            if latbounds is None or lonbounds is None:
                raise CDMSError, 'No boundary data is available for grid %s'%self.id
            if (transpose is not None) and (transpose[1]==0):
                latbounds = numpy.transpose(latbounds, (1,0,2))
                lonbounds = numpy.transpose(lonbounds, (1,0,2))
            print latbounds.shape
            mesh = numpy.zeros((self.size(),2,latbounds.shape[-1]),latbounds.dtype.char)
            mesh[:,LAT,:] = numpy.reshape(latbounds,(self.size(),latbounds.shape[-1]))
            mesh[:,LON,:]  = numpy.reshape(lonbounds,(self.size(),latbounds.shape[-1]))
            self._mesh_ = mesh
        return self._mesh_

    def _getShape (self):
        return self._lataxis_.shape

    # Don't try to generate bounds for curvilinear grids
    def genBounds(self):
        return (None, None)

    # Get the n-th index axis. naxis is 0 or 1.
    def getAxis(self, naxis):
        return self._lataxis_.getAxis(naxis)

    def getMask(self):
        """Get the mask array, if any, otherwise None is returned."""
        if self._maskVar_ is None:
            return self._tempMask_
        else:
            return self._maskVar_[:]

    def size(self):
        return self._lataxis_.size()

    def writeScrip(self, cufile, gridTitle=None):
        """Write a grid to a SCRIP file.
        cufile is a Cdunif file, NOT a CDMS file.
        gridtitle is a string identifying the grid.
        """
        import copy

        lat = numpy.ma.filled(self._lataxis_)
        lon = numpy.ma.filled(self._lonaxis_)
        blat, blon = self.getBounds()
        mask = self.getMask()

        ni, nj = self.shape
        if mask is None:
            mask = numpy.ones((ni, nj), numpy.int32)
        else:
            tmp = 1 - mask
            mask[:] = tmp.astype(mask.dtype.char)
            mask = mask.astype(numpy.int32)
        ngrid = ni*nj
        centerLat = copy.copy(lat)
        centerLat.shape = (ngrid,)
        centerLon = copy.copy(lon)
        centerLon.shape = (ngrid,)
        mask.shape = (ngrid,)

        clat = numpy.ma.filled(copy.copy(blat))
        clat.shape = (ngrid,4)
        clon = numpy.ma.filled(copy.copy(blon))
        clon.shape = (ngrid,4)

        # Write the file
        if gridTitle is None:
            gridTitle = self.id
        cufile.title = gridTitle
        cufile.createDimension("grid_size", ngrid)
        cufile.createDimension("grid_corners", 4)
        cufile.createDimension("grid_rank", 2)
        griddims = cufile.createVariable("grid_dims", 'i', ("grid_rank",))
        gridcenterlat = cufile.createVariable("grid_center_lat", 'd', ("grid_size",))
        gridcenterlat.units = "degrees"
        gridcenterlon = cufile.createVariable("grid_center_lon", 'd', ("grid_size",))
        gridcenterlon.units = "degrees"
        gridimask = cufile.createVariable("grid_imask", 'i', ("grid_size",))
        gridimask.units = "unitless"
        gridcornerlat = cufile.createVariable("grid_corner_lat", 'd', ("grid_size","grid_corners"))
        gridcornerlat.units = "degrees"
        gridcornerlon = cufile.createVariable("grid_corner_lon", 'd', ("grid_size","grid_corners"))
        gridcornerlon.units = "degrees"

        griddims[:] = numpy.array([nj,ni], numpy.int32)
        gridcenterlat[:] = centerLat
        gridcenterlon[:] = centerLon
        gridimask[:] = mask
        gridcornerlat[:] = clat
        gridcornerlon[:] = clon

    def toGenericGrid(self, gridid=None):

        import copy
        from auxcoord import TransientAuxAxis1D
        from coord import TransientVirtualAxis
        from gengrid import TransientGenericGrid

        lat = numpy.ma.filled(self._lataxis_)
        latunits = self._lataxis_.units
        lon = numpy.ma.filled(self._lonaxis_)
        lonunits = self._lonaxis_.units
        blat, blon = self.getBounds()
        mask = self.getMask()

        ni, nj = self.shape
        ngrid = ni*nj
        centerLat = copy.copy(lat)
        centerLat.shape = (ngrid,)
        centerLon = copy.copy(lon)
        centerLon.shape = (ngrid,)
        if mask is not None:
            mask.shape = (ngrid,)

        cornerLat = numpy.ma.filled(copy.copy(blat))
        cornerLat.shape = (ngrid,4)
        cornerLon = numpy.ma.filled(copy.copy(blon))
        cornerLon.shape = (ngrid,4)

        iaxis = TransientVirtualAxis("cell",ngrid)

        lataxis = TransientAuxAxis1D(centerLat, axes=(iaxis,), bounds=cornerLat,
                                  attributes={'units':latunits}, id="latitude")
        lonaxis = TransientAuxAxis1D(centerLon, axes=(iaxis,), bounds=cornerLon,
                                  attributes={'units':lonunits}, id="longitude")
        grid = TransientGenericGrid(lataxis, lonaxis, id=gridid, tempmask=mask)

        return grid

    def toCurveGrid(self, gridid=None):
        if gridid is None:
            gridid = self.id
        result = self.clone()
        result.id = gridid
        return result

    def writeToFile(self, file):
        latvar = self._lataxis_.writeToFile(file)
        lonvar = self._lonaxis_.writeToFile(file)
        if self._maskVar_ is not None:
            maskid = "mask_"+self.id
            file.write(self._maskVar_, id=maskid)
            latvar.maskid = maskid
            lonvar.maskid = maskid
        return (latvar, lonvar)

    def writeg( self, file ):
        """Write self as a Gridspec file representing a curvilinear grid.
        The file, normally a CdmsFile, should already be open for writing
        and will be closed."""
        import time
        from tvariable import TransientVariable
        
        # Set attributes
        if ( hasattr(file,'Conventions') ):
            if ( file.Conventions.find('Gridspec')<0 ):
                file.Conventions = file.Conventions + ' Gridspec-0.0'
        else:
            file.Conventions = 'Gridspec-0.0'
        if ( hasattr(file,'gs_filetypes') ):
            if ( file.gs_filetypes.find('Curvilinear_Tile')<0 ):
                file.gs_filetypes = file.gs_filetypes + ' Curvilinear_Tile'
        else:
            file.gs_filetypes = 'Curvilinear_Tile'
        t=time.time()
        id=int((t-int(t))*1.0e9)
        file.gs_id = id
        file.gs_originalfilename = os.path.basename( file.id )

        newhistory = '\n' + time.ctime() + ' CDAT/CDMS AbstractCurveGrid'
        # ... The \n gives a newline in the CDMS Python and in the Cdunif C code
        # which gets called to write to a file.  Someplace in the NetCDF libraries,
        # or possibly the ncdump utility, the newline is converted back to a "\n"
        # string, so you don't see a newline when you view the file. If we want
        # a real newline as the CF specification says, the libraries or ncdump
        # will have to be changed.
        # ... In the future we may want to add more history information.
        file.history = getattr( self, 'history', '' ) + \
                       getattr( file, 'history', '' ) + newhistory

        # former tile variable and attributes
        if ( hasattr(self,'long_name') and self.long_name!=None ):
            file.long_name = self.long_name
        else:
            file.long_name = 'gridspec_tile'
        # gs_geometryType is no longer required of Gridspec files, but as yet
        # there is no other proposal for describing the geometry (July 2010)
        if ( hasattr(self,'gs_geometryType') and self.gs_geometryType!=None):
            file.gs_geometryType = self.gs_geometryType
        else:
            file.gs_geometryType = 'spherical'
        # gs_discretizationType is no longer required of Gridspec files, but it's
        # harmless and may come in useful
        if ( hasattr(self,'gs_discretizationType') and self.gs_discretizationType!=None ):
            file.gs_discretizationType = self.gs_discretizationType
        else:
            file.gs_discretizationType = 'logically_rectangular'

        file.gs_lonv = 'gs_x'
        file.gs_latv = 'gs_y'
    
        # Set up and write variables.  When written, cdms writes not only the arrays
        # but also their coordinates, e.g. gs_nip.
        
        x=self._lonaxis_
        if ( not hasattr(x,'units') ):
            print "Warning, no units found for longitude"
            x.units = 'degree_east'
        if ( not hasattr(x,'standard_name') ):
            print "Warning, no standard_name found for longitude axis"
            x.standard_name = 'longitude'
        if ( x.standard_name == 'geographic_longitude'):
            # temporary for updating test files
            x.standard_name = 'longitude'
        x.id = file.gs_lonv
        # _lonaxis_ is a TransientAxis2D, hence a TransientVariable
        # But I don't know where the attribute _TransientVariable__domain comes from
        
        y=self._lataxis_
        if ( not hasattr(y,'units') ):
            print "Warning, no units found for latitude"
            y.units = 'degree_north'
        if ( not hasattr(y,'standard_name') ):
            print "Warning, no standard_name found for latitude axis"
            y.standard_name = 'latitude'
        if ( y.standard_name == 'geographic_latitude'):
            # temporary for updating test files
            y.standard_name = 'latitude'
        y.id = file.gs_latv

        if( not hasattr(x,'_TransientVariable__domain') ):
            # There probably doesn't exist enough information to write a correct
            # grid, but this will help.
            x._TransientVariable__domain = [ (x,), (y,) ]
        x._TransientVariable__domain[0][0].id='gs_njp'
        x._TransientVariable__domain[1][0].id='gs_nip'
        if ( not hasattr(y,'_TransientVariable__domain') ) :
            # There probably doesn't exist enough information to write a correct
            # grid, but this will help.
            y._TransientVariable__domain = [ (x,), (y,) ]
        y._TransientVariable__domain[0][0].id='gs_njp'
        y._TransientVariable__domain[1][0].id='gs_nip'

        file.write(x)
        file.write(y)
        file.close()
    
    def write_gridspec( self, filename ):
        """writes this grid to a Gridspec-compliant file, or does nothing if there is
        already a known file corresponding to this grid.  The filename should be a
        complete path."""
        # This method was never completed because the libCF functionality I had planned to
        # use never appeared (yet).
        # The functionality (other than checking gsfile) is now done by the writeg
        # method above.
        if ( not hasattr( self, "gsfile" ) ):
            self.gsfile=None
            self.gspath=None
        if ( self.gsfile!=None ):
            return ( tcg.gsfile, tcg.gspath )
        else:
            raise RuntimeError, 'The libCF/Gridspec API does not provide for writing CurveGrids<<<'

    def init_from_gridspec( self, filename ):
        """reads to grid from a Gridspec-compliant file.  The filename should be a
        complete path.  The contents of the file may overwrite data in the existing
        grid object."""
        # - This is really a kind of init function.  The __init__ function should
        # determine what kind of initialization is being done (from a file, from
        # another object, from arguments specifying the contents e.g. axes) and branch
        # to call a method such as this one.
        # - Another way to read a file is with the standard CDMS
        # pattern        file=cdms2.open(...);   g=file('grid') or g=file('')
        try:
            f = cdms2.open( filename )
        except IOError:
            print "Cannot open grid file for reading: ", filename
            return
        init_from_gridspec_file( self, f )
        f.close()

    def init_from_gridspec_file( self, f ):
        """reads to grid from a Gridspec-compliant file, f.  This f should be a
        CdmsFile object, already open for reading.  The contents of the file may
        overwrite data in the existing grid object."""
        # As for the above init_from_gridspec method, this is really a kind of
        # init function and should be called from __init__ .
        ax, ay, gs_attr = f.gridspec_file_contents()
        # ... gridspec_file_contents is defined in dataset.py
        self.__init__( ay, ax )
        self.gsfile = gs_attr['filebase']
        self.gspath = gs_attr['filepath']
        self.long_name = gs_attr['long_name']
        # gs_geometryType is no longer required of Gridspec files, but as yet
        # there is no other proposal for describing the geometry (July 2010)
        self.gs_geometryType = gs_attr['gs_geometryType']
        # gs_discretizationType is no longer required of Gridspec files, but it's
        # harmless and may come in useful
        self.gs_discretizationType = gs_attr['gs_discretizationType']
        return self

    def subSlice(self, *specs, **keys):
        """Get a transient subgrid based on an argument list <specs> of slices."""

        newlat = self._lataxis_.subSlice(*specs, **keys)
        newlon = self._lonaxis_.subSlice(*specs, **keys)
        if self._maskVar_ is None:
            newmask = None
        else:
            newmask = self._maskVar_.subSlice(*specs, **keys)

        result = TransientCurveGrid(newlat, newlon, maskvar=newmask)
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
        jaxis = self._lataxis_.getAxis(1)
        k = 0
        i = j = -1
        for d in domainlist:
            if d is iaxis:
                inewaxis = newaxislist[k]
                islice = slicelist[k]
                i = k
            if d is jaxis:
                jnewaxis = newaxislist[k]
                jslice = slicelist[k]
                j = k
            k += 1

        if i==-1 or j==-1:
            raise RuntimeError, 'Grid lat/lon domains do not match variable domain'

        return ((islice, jslice), (inewaxis, jnewaxis))

    def getIndex(self):
        """Get the grid index"""
        if self._index_ is None:
            latlin = numpy.ravel(numpy.ma.filled(self._lataxis_))
            lonlin = numpy.ravel(numpy.ma.filled(self._lonaxis_))
            self._index_ = bindex.bindexHorizontalGrid(latlin, lonlin)

        return self._index_

    def intersect(self, spec):
        """Intersect with the region specification.

        'spec' is a region specification of the form defined in the grid module.

        Returns (mask, indexspecs) where
        'mask' is the mask of the result grid AFTER self and region spec are interested.
        'indexspecs' is a list of index specifications suitable for slicing a
          variable with the given grid.
        """

        ni, nj = self.shape
        index = self.getIndex()
        latspec = spec[CoordTypeToLoc[LatitudeType]]
        lonspec = spec[CoordTypeToLoc[LongitudeType]]
        latlin = numpy.ravel(numpy.ma.filled(self._lataxis_))
        lonlin = numpy.ravel(numpy.ma.filled(self._lonaxis_))
        points = bindex.intersectHorizontalGrid(latspec, lonspec, latlin, lonlin, index)
        if len(points)==0:
            raise CDMSError, 'No data in the specified region, longitude=%s, latitude=%s'%(`lonspec`, `latspec`)

        fullmask = numpy.ones(ni*nj)
        numpy.put(fullmask, points, 0)
        fullmask = numpy.reshape(fullmask, (ni,nj))
        
        iind = points/nj
        jind = points - iind*nj
        imin, imax, jmin, jmax = (min(iind), max(iind)+1, min(jind), max(jind)+1)
        submask = fullmask[imin:imax, jmin:jmax]

        yid = self.getAxis(0).id
        xid = self.getAxis(1).id
        indexspecs = {yid:slice(imin,imax), xid:slice(jmin,jmax)}

        return submask, indexspecs

    def getAxisList(self):
        return (self._lataxis_.getAxis(0), self._lataxis_.getAxis(1))

    def isClose(self, g):
        """Return 1 iff g is a grid of the same type and shape. A real element-by-element
        comparison would be too expensive here."""
        if g is None:
            return 0
        elif self.shape != g.shape:
            return 0
        elif not isinstance(g, AbstractCurveGrid):
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
        for i in range(2):
            if selfaxes[i] not in axes:
                missing.append(i)
                result = None
            
        # Some of the grid axes are not in the 'axes' list
        if result is None:
            result = self.clone()
            used = []                   # axes already matched
            for i in missing:
                for item in axes:
                    if (item not in used) and len(selfaxes[i])==len(item) and allclose(selfaxes[i], item):
                        result._lataxis_.setAxis(i,item)
                        result._lonaxis_.setAxis(i,item)
                        used.append(item)
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
            alatflat = numpy.ravel(alat)
            alonflat = numpy.ravel(alon)
            self._flataxes_ = (alatflat, alonflat)
        return self._flataxes_
    shape = property(_getShape,None)
    
## PropertiedClasses.set_property (AbstractCurveGrid, 'shape', 
##                                   AbstractCurveGrid._getShape, nowrite=1,
##                                   nodelete=1)

class DatasetCurveGrid(AbstractCurveGrid):

    def __init__(self, latAxis, lonAxis, id, parent=None, maskvar=None, tempmask=None, node=None):
        """Create a file curvilinear grid.
        """
        AbstractCurveGrid.__init__(self, latAxis, lonAxis, id, maskvar, tempmask, node)
        self.parent = parent

    def __repr__(self):
        return "<DatasetCurveGrid, id: %s, shape: %s>"%(self.id, `self.shape`)

class FileCurveGrid(AbstractCurveGrid):

    def __init__(self, latAxis, lonAxis, id, parent=None, maskvar=None, tempmask=None, node=None):
        """Create a file curvilinear grid.
        """
        AbstractCurveGrid.__init__(self, latAxis, lonAxis, id, maskvar, tempmask, node)
        self.parent = parent

    def __repr__(self):
        return "<FileCurveGrid, id: %s, shape: %s>"%(self.id, `self.shape`)

class TransientCurveGrid(AbstractCurveGrid):

    grid_count = 0

    def __init__(self, latAxis, lonAxis, id=None, maskvar=None, tempmask=None):
        """Create a file curvilinear grid.
        """
        if id is None:
            TransientCurveGrid.grid_count += 1
            id = 'grid_' + str(TransientCurveGrid.grid_count)
        AbstractCurveGrid.__init__(self, latAxis, lonAxis, id, maskvar, tempmask)

    def __repr__(self):
        return "<TransientCurveGrid, id: %s, shape: %s>"%(self.id, `self.shape`)

    def toCurveGrid(self, gridid=None):
        if gridid is None:
            result = self
        else:
            result = self.clone()
            result.id = gridid
        return result

def readScripCurveGrid(fileobj, dims, whichType, whichGrid):
    """Read a 'native' SCRIP grid file, returning a transient curvilinear grid.
    fileobj is an open CDMS dataset or file object.
    dims is the grid shape.
    whichType is the type of file, either "grid" or "mapping"
    if whichType is "mapping", whichGrid is the choice of grid, either "source" or "destination"
    """
    import string
    from coord import TransientAxis2D

    if 'S' in fileobj.variables.keys():
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
    ni = dims[1]
    nj = dims[0]
    gridshape = (ni, nj)
    boundsshape = (ni, nj, ncorners)
    if hasattr(cornerLat, 'units') and string.lower(cornerLat.units)[0:6]=='radian':
        cornerLat = (cornerLat*(180.0/numpy.pi)).reshape(boundsshape)
        cornerLon = (cornerLon*(180.0/numpy.pi)).reshape(boundsshape)
    else:
        cornerLat = cornerLat.reshape(boundsshape)
        cornerLon = cornerLon.reshape(boundsshape)

    iaxis = TransientVirtualAxis("i",ni)
    jaxis = TransientVirtualAxis("j",nj)

    if vardict.has_key(gridMaskName):
        # SCRIP convention: 0 for invalid data
        # numpy.ma convention: 1 for invalid data
        mask = 1 - fileobj(gridMaskName)
        mask = mask.reshape(gridshape)
    else:
        mask = None
        
    if vardict.has_key(gridCenterLatName):
        centerLat = fileobj(gridCenterLatName).reshape(gridshape)
        gclat = fileobj[gridCenterLatName]
        if hasattr(gclat, "units") and string.lower(gclat.units)=='radians':
            centerLat *= (180.0/numpy.pi)
    else:
        centerLat = cornerLat[:,:,0]

    if vardict.has_key(gridCenterLonName):
        centerLon = fileobj(gridCenterLonName).reshape(gridshape)
        gclon = fileobj[gridCenterLonName]
        if hasattr(gclon, "units") and string.lower(gclon.units)=='radians':
            centerLon *= (180.0/numpy.pi)
    else:
        centerLon = cornerLon[:,:,0]

    if hasattr(fileobj,titleName):
        gridid = getattr(fileobj, titleName)
        gridid = string.replace(string.strip(gridid), ' ','_')
    else:
        gridid="<None>"

    lataxis = TransientAxis2D(centerLat, axes=(iaxis, jaxis), bounds=cornerLat,
                              attributes={'units':'degrees_north'}, id="latitude")
    lonaxis = TransientAxis2D(centerLon, axes=(iaxis, jaxis), bounds=cornerLon,
                              attributes={'units':'degrees_east'}, id="longitude")
    grid = TransientCurveGrid(lataxis, lonaxis, id=gridid, tempmask=mask)

    return grid
