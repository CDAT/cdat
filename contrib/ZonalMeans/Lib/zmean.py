# Adapted for numpy/ma/cdms2 by convertcdms.py
# <Function compute_array_of_lat_bands>=                                  
import numpy
import _gengridzmean
import cdms2
import MV2
import genutil

def compute_array_of_lat_bands (delta_band, nbands):
    """
!   -------------------------------------------------------------------
!   PURPOSE
!   -------
!   From specified "bandwidth", this routine computes a set of latitudinal
!   bands for use with calculating zonal means
!
!   INPUT
!   -----
!   delta_band        [r] : Desired width of each latitudinal band
!   nbands            [i]  : Number of bands (based on delta_band)
!
!   OUTPUT
!   ------
!   nlines            [i]  : Number of lines defining all latitudinal bands
!                            between -90 and +90 degrees latitude
!   lineslat (nlines) [ra] : Array of lines defining latitudinal bands
!   midlat (nbands)   [ra] : Mid-point of each latitudinal band
!   bandset_id        [c]  : Character ID of this set of bands
!                            used for filename construction
!
"""
#
#   Set band width every x degrees, where x = delta_band
#   Here, bandlat(n) corresponds to   the minimum lat of each band, whereas
#         bandlat(n+1) corresponds to the maximum lat of each band

#   Invoke following line before calling this routine
#   nbands = int (180./delta_band)

#
    nlines = nbands + 1
    lineslat = numpy.arange (-90., 90. + delta_band, delta_band)

#   Determine mid-point of each latitudinal band
    midlat = (lineslat[0:-1] + lineslat[1:]) / 2.

#   Construct bounds_lat array that defines south and north limits,
#   respectively, for each latitudinal band
    bounds_midlat = numpy.transpose (numpy.array ((lineslat[0:-1], lineslat[1:])))

#   Convert REAL bandwidth to a CHARACTER variable, for filename construction
    bandset_id = '%3.1f' % delta_band

    return midlat, bounds_midlat, nlines, lineslat, bandset_id


# <Function get_basin_masks>=                                             

def get_ocmip_basin_masks (basinfilename, land_mask):

    """
!===========================================================================
! Routine to
! (1) Read in predefined basin mask file from OCMIP with 1 2-D spatial array
!     where each ocean grid points is identified by basin (0 = Atlantic,
!     1 = Pacific, 2 = Arctic, 3 = Marginal Seas [Med. Sea, Hudson Bay,
!     Red Sea], and 4 = Indian Ocean;
! (2) Rearrange information so that each basin above has its own 2-D spatial
!     array (BMASK) where that basin is identified as 1 and outside point
!     (both land and water) are identified as 0);
! (3) Combine information above to make same kind of 1/0 array to identify
!     two global masks (one WITH and one WITHOUT Marginal Seas); and
! (4) Combine information to make 2-D arrays for two combined basins
!     (Atlantic+Arctic; IndoPac).

Input arguments:
    basinfilename:  Pathname of basin NetCDF file
    land_mask:      Land mask at surface level where land is identified as 0

Output:
    basin_mask:     3D Array of 9 masks as described above

! James Orr, LSCE/CEA Saclay, CEA-CNRS and IPSL, 28 June 2000
!===========================================================================
"""

#   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#   Read OCMIP Basin mask file (input)
#   that uses the following indices to identify basins
#              0 = Atlantic
#              1 = Pacific
#              2 = Arctic
#              3 = Marginal Seas (Med. Sea, Hudson Bay, Red Sea)
#              4 = Indian Ocean
#   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    basin_file = cdms2.open (basinfilename)
    ocmip_bmask = basin_file ('BASIN').getValue()
    basin_file.close()

#   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#   Reassign basins to following
#      * 1 = atl       -> Atlantic
#      * 2 = pac       -> Pacific
#      * 3 = ind       -> Indian
#      * 4 = arc       -> Arctic
#      * 5 = mar       -> Marginal Seas
#      * 6 = glomar    -> Global (all, same as mask(:,:))
#      * 7 = glo       -> Global (without marginal Seas)
#      * 8 = atlarc    -> Combined Atlantic and Arctic
#      * 9 = indpac    -> IndoPac (i.e., combined Indian and Pacific masks)
#   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#   -------------------------------------------------------------
#   0) Reassign basin indicators for zonal mean program:
#      Position of INDICE -> corresponds to reassigned values;
#      Value of ibasin    -> corresponds to former indices (from basinfile)
#                            that are being reassigned
#   -------------------------------------------------------------
    # Make an array with same array type as input for faster comparison
    ibasin = numpy.array ((0, 1, 4, 2, 3), ocmip_bmask.dtype.char)

#   -------------------------------------------------------------
#   1) Initialize all basin_masks points to zero (all land and water points)
#   -------------------------------------------------------------
    output_shape = [9,] + list (land_mask.shape)
    basin_masks = numpy.zeros (output_shape, numpy.int)

#   ---------------------------------------------------------------
#   2) Assign masks for individual basins (atl, pac, ind, arc, mar)
#   ---------------------------------------------------------------
    for index_basin in range (len (ibasin)):
        basin_masks [index_basin,:,:] = (ocmip_bmask [:,:] == ibasin [index_basin])

#   -------------------------------------------------------------
#   3) Make Global Basin mask (with marginal seas, "glomar") ,
#      i.e.,  just the same as in surface Level "mask"
#      (1=ocean ; 0=land)
#   -------------------------------------------------------------
    land_mask_boolean = (land_mask [:,:] == 1.)
    basin_masks [5,:,:] = land_mask_boolean

#   ----------------------------------------------------------------
#   4) Make Global Basin mask (without marginal seas, "glo")
#   ----------------------------------------------------------------
    basin_masks [6,:,:] = numpy.where (ocmip_bmask == 3, 0, land_mask_boolean)

#   -------------------------------------------------------------
#   5) Make mask for 1st combined basin (atlarc)
#   -------------------------------------------------------------
    basin_masks [7,:,:] = numpy.logical_or (ocmip_bmask == 0, ocmip_bmask == 2)

#   -------------------------------------------------------------
#   6) Make masks for 2nd combined basin (indpac)
#   -------------------------------------------------------------
    basin_masks [8,:,:] = numpy.logical_or (ocmip_bmask == 1, ocmip_bmask == 4)
                                                             
    return basin_masks
#  END SUBROUTINE get_basin_masks


def compute(variable,delta_band=None,
            regions_masks=None,
            bounds_lon=None, bounds_lat=None, bounds_depth=None,
            area=None, volume=None):

    """ Computes zonal means for any grid
    Usage:
    areaband, zonxbar, zoninv, zonmask = compute(variable,delta_band=4.5,
            regions_masks=None,
            bounds_lon=None, bounds_lat=None, bounds_depth=None,
            area=None, volume=None)

    Input:
    variable: variable on which you want to compute zonal mean
    delta_band (optional):
          list representing the bounds of the latitudes band within which to compute the zonal means
          OR
          integer representing the width of each band (bands will then go from -90 to 90)
          DEFAULT VALUE:
          if "rectangular" grid, then the actual latitude bounds (if found)
          otherwise 4.5 degrees bands from -90 to 90
    region_mask: a 3D array with the same horizontal resolution as variable and an optional 3rd dimension representing various regions to mask, 1 means compute 0 means do not use this point. In any case the variable mask will be superposed onto this mask (cell where variable is masked/missing will not be used)
          DEFAULT VALUE: only 1 region with 1 everywhere (global)
    bounds_lat/bounds_lon: bounds for each latitudes/longitudes as returned by cdms2 getMesh function, i.e. 2D (space,nvertices), except here vertices must be 4
    bounds_depth: If variable as vertical dimnesion, pass the bounds depth here, if None passed will try to figure it out from getLevel().getBounds()
    area: 2D array that represents the area of each spatial grid cell, if None try and cdms2 is able to will try to get the weigfhts on its own.
    volume: for 3D (xyz) variables, you can pass volume then it will compute ratio of real volume vs. volume of depth x area the two should be equal except for partially masked grid cells.

    Output:
      areaband : Area (in m**2) of each latitude band, dimensions are: (region,level,latitude)
                  where the length of the latitude dim is the number of bands passed       
      zonxbar  : Zonal Mean Average in each band, dimensions are: (region,time,level,latitude)
                  where the length of the latitude dim is the number of bands passed
      zoninv   : Inventory of the quantity of the varialbe in each band, same dims as zonxbar

    """
##     print 'Dans compute!'
    grid=variable.getGrid()
    ## Deal with input variable
    variable=MV2.array(variable) # Just making sure
    oaxes=variable.getAxisList()
    axes=variable.getAxisList()
    vid=variable.id
    order=variable.getOrder(ids=1)
    osh=list(variable.shape)
    sh=list(osh)
    tim=variable.getTime()
    if tim is None: # No time?
        tim=cdms2.createAxis([1.],id='time')
        tim.units='months since 2000'
        tim.designateTime()
        axes.insert(0,tim)
        sh.insert(0,1)
        variable=MV2.reshape(variable,sh)
        variable.setAxisList(axes)
    lev=variable.getLevel()
    if lev is None: # No time?
        lev=cdms2.createAxis([1.],id='level')
        lev.designateLevel()
        axes.insert(1,lev)
        sh.insert(1,1)
        variable=MV2.reshape(variable,sh)
        variable.setAxisList(axes)
    # Ok now tries to determine lat/lon to reorder properly
    if grid is None:
        norder=order
        if bounds_lon is None and bounds_lat is None:
            raise 'Error you need to pass bounds lon/lat or have a cdms2 recognized grid on your variable'
    else:
        if isinstance(grid,cdms2.grid.TransientRectGrid):
            norder='...tzyx'
        else: # Ok "generalized grid"
            norder='...tz'
            a=grid.getAxisList()
            for n in a:
                norder+='(%s)' % n.id
    variable = variable(order=norder)
    ## Deal with mask
    mask=variable.mask
    if mask is numpy.ma.nomask:
        mask=numpy.ones(variable.shape[-3:],numpy.float32)
    else:
        while numpy.rank(mask)>3: # only keeps lev/lat/lon
            mask=mask[0]
        mask=1.-mask
        mask=mask.astype('f')

    if bounds_lon is None:
        bounds_lon=grid.getMesh()[...,1,:]
        gsh=list(grid.shape)
        gsh.append(bounds_lon.shape[-1])
        bounds_lon = numpy.reshape(bounds_lon,gsh)
    elif bounds_lon.shape[-3:-1]!=variable.shape[-2:]:
        raise 'Error variable and bounds_lon shapes are not compatible %s, %s' % (str(variable.shape), str(bounds_lon.shape))

    bounds_lon=numpy.array(bounds_lon,numpy.float)
    
    if bounds_lat is None:
        bounds_lat=grid.getMesh()[...,0,:]
        gsh=list(grid.shape)
        gsh.append(bounds_lat.shape[-1])
        bounds_lat = numpy.reshape(bounds_lat,gsh)
    elif bounds_lat.shape[-3:-1]!=variable.shape[-2:]:
        raise 'Error variable and bounds_lat shapes are not compatible %s, %s' % (str(variable.shape), str(bounds_lat.shape))
    bounds_lat=numpy.array(bounds_lat,numpy.float)

    if bounds_depth is None: # Didn't pass levels?
        l=variable.getLevel()
        bounds_depth=l.getBounds()
        if bounds_depth is None: # Still no levels?
            n=len(l)
            if n==1:
                bounds_depth = numpy.ones ((1,2), numpy.float)
                bounds_depth[0,0] = 0.
            else:
                bounds_depth = numpy.ones ((n,2), numpy.float)
                bounds_depth[:-1,1]=l[1:]-l[:-1]
                bounds_depth[1:,0]=l[1:]-l[:-1]
                bounds_depth[0,0] = l[0]-(bounds_depth[0,1]-l[0])
                bounds_depth[-1,1] = l[-1]+(l[-1]-bounds_depth[-1,0])
    bounds_depth=numpy.array(bounds_depth)

    ## Deals with area
    if area is None:
        if isinstance(grid,cdms2.grid.TransientRectGrid):
            a,b = grid.getWeights()
            area = a[...,None]*numpy.transpose(b)*509904363781790.69
        else: # Ok "generalized grid"
            raise "Area error you need to provide area array, couldn't compute it on my own"
    elif area.shape!=variable.shape[-2:]:
        raise 'Error variable and area shapes are not compatible %s, %s' % (str(variable.shape), str(area.shape))
    area=numpy.array(area)

    if volume is None:
        vsh=list(variable.shape[-2:])
        vsh.insert(0,1)
        depth = numpy.array(bounds_depth[:,1]-bounds_depth[:,0])
        volume=area[None,...]*depth[:,None,None]
    else:
        vsh=volume.shape
        if vsh[-2:]!=variable.shape[-2:]:
            raise 'Error variable and volume shapes are not compatible %s, %s' % (str(variable.shape), str(volume.shape))

    if regions_masks is None:
        bsh=list(variable.shape[-2:])
        bsh.insert(0,1)
        regions_masks=numpy.ones(bsh,numpy.int32)
        userregion = False
    else:
        bsh=regions_masks.shape
        if vsh[-2:]!=variable.shape[-2:]:
            raise 'Error variable and regions_masks shapes are not compatible %s, %s' % (str(variable.shape), str(regions_masks.shape))
        userregion = True


    ## Latitudes bands
    if delta_band is None:
        bandlat=None
        if isinstance(grid,cdms2.grid.TransientRectGrid):
            lat = grid.getLatitude()
            blat=lat.getBounds()
            if blat is not None:
                bandlat=list(blat[:,0])
                bandlat.append(blat[-1,1])
                midlat = lat[:]
                bounds_midlat = blat
        if bandlat is None:
            nbands = 36  # 4.5 degrees bands
            midlat, bounds_midlat, nlines, bandlat, bandset_id = compute_array_of_lat_bands (4.5, nbands)
    elif isinstance(delta_band,(int,float)):
        # Compute number of bands
        nbands = int(180./delta_band + 0.5)  # Compute nearest integer

        # These arrays define the latitudinal bands;
        # Also provide 3-character name (bandset_id) based on band width
        midlat, bounds_midlat, nlines, bandlat, bandset_id = compute_array_of_lat_bands (delta_band, nbands)
    elif isinstance(delta_band, (tuple,list)):
        check_list = [isinstance(i,(int,float)) for i in delta_band]
        if not reduce(lambda x,y: x and y,check_list):
            raise 'Error delta_band must be list or tuple of numbers'
        for l in delta_band:
            if not -90.<=l<=90.:
                raise 'Error delta_band values must be between -90 and 90'
##         # Add latitudes -90 and 90
##         if delta_band[0] != -90:
##             delta_band.insert (0, -90)
##         if delta_band[-1] != 90:
##             delta_band.append (90)

        nlines = len (delta_band)
        nbands = nlines - 1
        bandlat = numpy.array (delta_band, numpy.float)

        # Determine mid-point of each latitudinal band
        midlat = (bandlat[0:-1] + bandlat[1:]) / 2.

        # Construct bounds_lat array that defines south and north limits,
        # respectively, for each latitudinal band
        bounds_midlat = numpy.transpose (numpy.array ((bandlat[0:-1], bandlat[1:])))
        bandset_id = 'user_defined'
    else:
        raise 'Error delta_band must be integer or list of latitudes'
    
        
    # Compute zonal means                                                
    # Determine pathname of file that stores fractionnal area information
#    farea_filename = os.path.join (farea_dir, 'fareaz_' + bandset_id + '_' + model_id + '.bnd')

    # Set the type of input data to simple precision
    variable = numpy.asfortranarray(numpy.ascontiguousarray(numpy.transpose(variable.astype (numpy.float32).filled(0))))
    volume = numpy.asfortranarray(numpy.ascontiguousarray(numpy.transpose(volume.astype (numpy.float32))))
    if mask.shape!=():
        mask = numpy.asfortranarray(numpy.ascontiguousarray(numpy.transpose(mask.astype (numpy.float32))))
    regions_masks = numpy.asfortranarray(numpy.ascontiguousarray(numpy.transpose(regions_masks.astype (numpy.float32))))
    bounds_lon = numpy.asfortranarray(numpy.ascontiguousarray(numpy.transpose(bounds_lon.astype (numpy.float32))))
    bounds_lat = numpy.asfortranarray(numpy.ascontiguousarray(numpy.transpose(bounds_lat.astype (numpy.float32))))
    bounds_depth = numpy.asfortranarray(numpy.ascontiguousarray(bounds_depth.astype (numpy.float32)))
    area = numpy.asfortranarray(numpy.ascontiguousarray(numpy.transpose(area.astype (numpy.float32))))
    bandlat = numpy.asfortranarray(numpy.ascontiguousarray(numpy.transpose(bandlat)).astype (numpy.float32))

    
    sh=variable.shape
    imt,jmt,kmt,nt = sh[:4]
    sh=mask.shape
    kmt_grid=sh[-1]
    sh=regions_masks.shape
    iomax=sh[-1]
    vl=volume.shape[-1]
##     print 'Ok jusque ici',bounds_depth.shape
##     print 'contiguous',variable.flags.contiguous
##     print regions_masks
##     raw_input('press enter')
    areaband, zonxbar, zoninv, zonmask = \
        _gengridzmean.zonebasin (variable,bounds_lon, bounds_lat, bounds_depth,
        mask, regions_masks,
        area,volume,
        bandlat,imt,jmt,kmt,nt,kmt_grid,iomax,vl)
##         _gengridzmean.zonebasin (variable,imt,jmt,kmt,nt,kmt_grid,iomax,
##         bounds_lon, bounds_lat, tbd,
##         mask, regions_masks,
##         area, vl,volume,
##         bandlat)
##     raw_input('press enter again')

    areaband = numpy.transpose(areaband)
    zonxbar = numpy.transpose(zonxbar)
    zoninv = numpy.transpose(zoninv)
    zonmask = numpy.transpose(zonmask)

##     print zonxbar.shape
    ## Now puts back on it the "decoration"
    Axes=[]
    dimspop=[]
    if userregion:
        if cdms2.isVariable(regions_masks):
            Axes.append(regions_masks.getAxis(0))
        elif MV2.rank(regions_masks)==3:
            Axes.append(cdms2.createAxis(range(regions_masks.shape[-1]),id='regions'))
    else:
        dimspop.append(0)

    found_time = False
    for ax in oaxes:
        if ax.isTime():
            Axes.append(ax)
            found_time = True
    if found_time is False and nt==1:
        dimspop.insert(0,1) # We can remove the time
        
    found_level = False
    for ax in oaxes:
        if ax.isLevel():
            Axes.append(ax)
            found_level = True

    if found_level is False and kmt==1:
        dimspop.insert(0,2) # We can remove the time

    for i in dimspop:
        if i==0 :
            areaband = numpy.add.reduce(areaband,i)
            zonmask = numpy.add.reduce(zonmask,i)
        elif i>1:
            areaband = numpy.add.reduce(areaband,i-1)            
            zonmask = numpy.add.reduce(zonmask,i-1)
        zonxbar = numpy.add.reduce(zonxbar,i)
        zoninv = numpy.add.reduce(zoninv,i)
        

    ax=cdms2.createAxis(midlat,id='latitude',bounds=bounds_midlat)
    ax.designateLatitude()
    ax.units='degrees_north'

    Axes.append(ax)


    Mask=numpy.resize(1-zonmask,zonxbar.shape)
    zonxbar = MV2.array(zonxbar, id=vid, axes=Axes,mask=Mask,fill_value=1.e20)

    zoninv = MV2.array(zoninv, id=vid+'_inventory',axes=Axes,mask=Mask,fill_value=1.e20)

    for ax in Axes:
        if ax.isTime():
            Axes.remove(ax)

##     if not userregion:
##         Axes.pop(0)
        
    areaband = MV2.array(areaband, id='area', axes=Axes,mask=1-zonmask,fill_value=1.e20)

    #zonmask = MV2.array(zonmask, id='zonmask', axes=Axes)
    
    return areaband, zonxbar, zoninv
