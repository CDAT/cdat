'''
Created on Sep 18, 2013

@author: tpmaxwel
'''
import sys, math
import numpy
import cdms2, cdutil

class InterfaceType:
    ClimatePointCloud = 0
    InfoVis = 1


def isNone(obj):
    return ( id(obj) == id(None) )

def lsize( axis ):
    try:     return axis.size()
    except:  return axis.size
    
def getVarAttribute( var, attribute_name_list ):
    for attribute_name in attribute_name_list:
        try:
            attr_val =  getattr( var, attribute_name )
            return attr_val
        except AttributeError:
            attr_val = var.attributes.get( attribute_name )
            if attr_val: return attr_val
    return None

class PlotType:
    Planar = 0
    Spherical = 1
    List = 0
    Grid = 1
    LevelAliases = [ 'isobaric', "layers", "interfaces"  ]
    
    @classmethod
    def validCoords( cls, lat, lon ):
        return ( id(lat) <> id(None) ) and ( id(lon) <> id(None) )
    
    @classmethod
    def isLevelAxis( cls, pid ):
        lname = pid.lower()
        if ( lname.find('lev')  >= 0 ): return True
        if ( lname.find('bottom') >= 0 ) and ( lname.find('top') >= 0 ): return True
        if pid in cls.LevelAliases: return True
        return False    

    @classmethod
    def getPointsLayout( cls, grid ):
        if grid <> None:
            if (grid.__class__.__name__ in ( "RectGrid", "TransientRectGrid", "FileRectGrid") ): 
                return cls.Grid
        return cls.List  

class MultiVarPointCollection():

    def __init__( self ):
        self.iTimeStep = 0
        self.point_data = None
        self.vtk_planar_points = None                                  
        self.cameraOrientation = {}
        self.topo = PlotType.Planar
        self.lon_data = None
        self.lat_data = None 
        self.z_scaling = None
        self.hgt_var = None 
        self.metadata = {}
        self.istart = 0
        self.istep = 1
        self.roi =None
        self.point_data_arrays = {}
        self.vrange = {}
        self.thresholded_range = {}
        self.point_layout = None
        self.axis_bounds = {}
        self.threshold_target = None
        self.vertical_bounds = None
        self.maxStageHeight = 100.0
        self.var_data_cache = {}
        self.vars = {} 
        self.thresholdTargetType = None
        self.selected_index_array = None

    def setROI( self, ROI ):
        self.roi = ROI
        self.initPoints()
               
    def configure(self, **args ):
        self.maxStageHeight = args.get('maxStageHeight', self.maxStageHeight )
        self.roi = args.get('roi', None )
        
    def getGridType(self):
        return self.point_layout
    
    def getCoordIndex( self, var, coord ):
        try:
            axis_order = var.getOrder()
            return axis_order.index(coord)
        except ValueError, err:
            print>>sys.stderr, "Can't find axis %s in axis order spec '%s' " % ( coord, axis_order )


    def getCoordDataBlock( self, var ):
        return None
       
    def getDataBlock( self, var ):
        np_var_data_block = None
        iTimeIndex = self.getCoordIndex( var, 't' )
        if iTimeIndex <> 0:  
            print>>sys.stderr, "Unimplemented axis order: %s " % var.getOrder()
        else:
            if self.lev == None:
                if len( var.shape ) == 2:
                    np_var_data_block = var[ self.iTimeStep, self.istart::self.istep ].data
                    if self.roi_mask <> None:
                        np_var_data_block = numpy.compress( self.roi_mask, np_var_data_block )
                elif len( var.shape ) == 3:
                    np_var_data_block = var[ self.iTimeStep, :, self.istart::self.istep ].data
                    np_var_data_block = np_var_data_block.reshape( [ np_var_data_block.shape[0] * np_var_data_block.shape[1], ] )
                self.nLevels = 1
            else:
                iLevIndex = self.getCoordIndex( var, 'z' )
                if len( var.shape ) == 3: 
                    if iLevIndex == 1:              
                        np_var_data_block = var[ self.iTimeStep, :, self.istart::self.istep ].data
                    elif iLevIndex == 2:     
                        np_var_data_block = var[ self.iTimeStep, self.istart::self.istep, : ].data
                        np_var_data_block = numpy.swapaxes( np_var_data_block, 0, 1 )
                    else:
                        print>>sys.stderr, "Unimplemented axis order: %s " % var.getOrder()
                    if not isNone( np_var_data_block ):
                        if not isNone( self.roi_mask ): 
                            np_var_data_block = numpy.compress( self.roi_mask, np_var_data_block, axis = 1 )
                elif len( var.shape ) == 4:
                    lev_data_arrays = []
                    for ilev in range( var.shape[1] ):                       
                        data_z_slice = var[ self.iTimeStep, ilev ].flatten()
                        lev_data_arrays.append( data_z_slice[self.istart::self.istep] )
                    np_var_data_block = numpy.concatenate( lev_data_arrays ).astype( numpy.float32 )     
#            print " GetDataBlock, var.shape = %s, grid = %s, ts = %d, newshape = %s " % ( str(var.shape), str((self.istart,self.istep)), self.iTimeStep, str(np_var_data_block.shape) )
                        
            if not isNone( np_var_data_block ):                
                if self.missing_value:  np_var_data_block = numpy.ma.masked_equal( np_var_data_block, self.missing_value, False ).flatten()
                else:                   np_var_data_block = np_var_data_block.flatten()
                
        return np_var_data_block

    def getTimeseries( self, var, pointIndex, iLevel = -1 ):
        np_var_data_block = None
        if self.lev == None:
            np_var_data_block = var[ :, pointIndex ].data
        else:
            iLevIndex = self.getCoordIndex( var, 'z' )
            if len( var.shape ) == 3: 
                if iLevIndex == 1:              
                    np_var_data_block = var[ :, iLevIndex, pointIndex ].data
                elif iLevIndex == 2:     
                    np_var_data_block = var[ :, pointIndex, iLevIndex ].data
                    np_var_data_block = numpy.swapaxes( np_var_data_block, 0, 1 )
                else:
                    print>>sys.stderr, "Unimplemented axis order: %s " % var.getOrder()
            
        return np_var_data_block
    
    def processCoordinates( self, lat, lon ):
#        print "Process Coordinates, lat = %s%s, lon = %s%s " % ( lat.id, str(lat.shape), lon.id, str(lon.shape)  )
        nz = len( self.lev ) if self.lev else 1
        self.n_input_points = lsize(lat) * nz if ( self.point_layout == PlotType.List ) else lsize(lat) * lsize(lon) * nz
        if self.istep <= 0: self.istep = max( self.n_input_points / self.max_points, 1 )
        if lon.__class__.__name__ == "TransientVariable":
            self.lat_data = lat.flatten()[self.istart::self.istep] if ( self.point_layout == PlotType.List ) else lat.flatten()[::]
            self.lon_data = lon.flatten()[self.istart::self.istep] 
        else:
            self.lat_data = lat[self.istart::self.istep] if ( self.point_layout == PlotType.List ) else lat[::]
            self.lon_data = lon[self.istart::self.istep] 
        if ( self.point_layout == PlotType.List ) and ( self.roi <> None ):
            if ( self.roi[2] <= 0.0 ) and ( self.lon_data.data.min() >= 0.0 ):
                self.roi = [ self.roi[0]+360.0, self.roi[1], self.roi[2]+360.0, self.roi[3] ]
            if ( self.roi[0] > 180.0 ) and ( self.lon_data.data.max() <= 180.0 ):
                self.roi = [ self.roi[0]-360.0, self.roi[1], self.roi[2]-360.0, self.roi[3] ]
            lat_roi_mask = numpy.logical_and( self.lat_data > self.roi[1], self.lat_data < self.roi[3] )
            lon_roi_mask = numpy.logical_and( self.lon_data > self.roi[0], self.lon_data < self.roi[2] )
            self.roi_mask = numpy.logical_and( lat_roi_mask, lon_roi_mask )
            if self.roi_mask.any():
                self.lat_data = numpy.compress( self.roi_mask, self.lat_data )
                self.lon_data = numpy.compress( self.roi_mask, self.lon_data )
            else:
                print>>sys.stderr, "Ignoring empty ROI"
                self.roi_mask = None
        else: 
            self.roi_mask = None
        if self.lat_data.__class__.__name__ == "TransientVariable":
            self.lat_data = self.lat_data.data
            self.lon_data = self.lon_data.data        
        try:
            if lat.units == "radians":
                radian_conversion_factor = ( 180.0 / math.pi )
                self.lat_data = self.lat_data * radian_conversion_factor
                self.lon_data = self.lon_data * radian_conversion_factor                    
        except: pass
        xmax, xmin = self.lon_data.max(), self.lon_data.min()
        ymax, ymin = self.lat_data.max(), self.lat_data.min()
        self.axis_bounds[ 'x' ] = ( xmin, xmax )
        self.axis_bounds[ 'y' ] = ( ymin, ymax )
        self.xcenter =  ( xmax + xmin ) / 2.0       
        self.xwidth =  ( xmax - xmin ) 
        self.ycenter =  ( ymax + ymin ) / 2.0       
        self.ywidth =  ( ymax - ymin ) 
        return lon, lat

    def getNumberOfInputPoints(self): 
        return self.n_input_points
    
    def getNumberOfPoints(self): 
        return len( self.point_data_arrays['x'] ) 
    
    def levelsAreAscending(self):
        if self.lev == None: return True
        lev_positive_direction = self.lev.attributes.get('positive','up')
        lev_values_ascending = ( self.lev[-1] > self.lev[0] )
        if ( lev_positive_direction == 'down' ):
            return not lev_values_ascending
        else:
            return lev_values_ascending
    
    def setPointHeights( self, **args ):
        if self.lev == None:
            stage_height = 0.0
            if self.point_layout == PlotType.List:
                z_data = numpy.empty( self.lon_data.shape, self.lon_data.dtype ) 
            elif self.point_layout == PlotType.Grid: 
                z_data = numpy.empty( [ self.lon_data.shape[0] * self.lat_data.shape[0] ], self.lon_data.dtype ) 
            z_data.fill( stage_height )
            self.point_data_arrays['z'] = z_data
        else: 
            height_varname = args.get( 'height_var', None )
            z_scaling = args.get( 'z_scale', 1.0 )
            self.data_height = args.get( 'data_height', None )
            ascending = self.levelsAreAscending()
#            print "setPointHeights: z_scaling=%s, stage_height=%s" % ( str(z_scaling), str(self.maxStageHeight) )
            stage_height = ( self.maxStageHeight * z_scaling )
            
            nz = len( self.lev ) if self.lev else 1
            if height_varname and (height_varname <> self.hgt_var) and (height_varname <> 'Levels' ):
                hgt_var = self.getProcessedVariable( height_varname )
                if hgt_var:
                    self.hgt_var = height_varname
                    np_hgt_var_data_block = self.getDataBlock(hgt_var)
                    zdata = np_hgt_var_data_block.astype( numpy.float32 ) 
    #                print " setPointHeights: zdata shape = %s " % str( zdata.shape ); sys.stdout.flush()
                    self.vertical_bounds = ( zdata.min(), zdata.max() )  
                    if self.data_height == None: self.data_height = ( self.vertical_bounds[1] - self.vertical_bounds[0] )
                    self.point_data_arrays['z'] = zdata * ( stage_height / self.data_height ) 
                else:
                    print>>sys.stderr, "Can't find height var: %s " % height_varname
            else:
                if ( z_scaling <> self.z_scaling ) or ( self.roi <> None ) or ( (height_varname <> self.hgt_var) and (height_varname == 'Levels' ) ):
                    self.z_scaling = z_scaling
                    if height_varname: self.hgt_var = height_varname
                    np_points_data_list = []
                    zstep = stage_height / nz
                    for iz in range( nz ):
                        zvalue = iz * zstep
                        if self.point_layout == PlotType.List:
                            z_data = numpy.empty( self.lon_data.shape, self.lon_data.dtype ) 
                        elif self.point_layout == PlotType.Grid: 
                            z_data = numpy.empty( [ self.lon_data.shape[0] * self.lat_data.shape[0] ], self.lon_data.dtype ) 
                        z_data.fill( zvalue )
                        if ascending: np_points_data_list.append( z_data.flat )
                        else: np_points_data_list.insert( 0, z_data.flat )
        #            print "Sample z data value: %s" % str( np_points_data_list[0][0] )
                    self.point_data_arrays['z'] = numpy.concatenate( np_points_data_list ).astype( numpy.float32 ) 
        self.vertical_bounds =  ( 0.0, stage_height )  
        self.axis_bounds[ 'z' ] = self.vertical_bounds
        
    def getAxisBounds( self, axis=None ): 
        return self.getBounds() if ( axis == None ) else self.axis_bounds[ axis ]                 

    def computePoints( self, **args ):
        nz = len( self.lev ) if self.lev else 1
        if self.point_layout == PlotType.List:
            self.point_data_arrays['x'] = numpy.tile( self.lon_data.astype( numpy.float32 ), nz ) 
            self.point_data_arrays['y'] = numpy.tile( self.lat_data.astype( numpy.float32 ), nz )  
        elif self.point_layout == PlotType.Grid: 
            grid_data_x = numpy.tile( self.lon_data, self.lat_data.shape[0] )  
            grid_data_y = numpy.repeat( self.lat_data, self.lon_data.shape[0] )  
            self.point_data_arrays['x'] = numpy.tile( grid_data_x, nz )  
            self.point_data_arrays['y'] = numpy.tile( grid_data_y, nz )  
        
    def getBounds(self):
        return self.axis_bounds[ 'x' ] + self.axis_bounds[ 'y' ] + self.axis_bounds[ 'z' ] 

    def getPointsLayout( self, var ):
        return PlotType.getPointsLayout( var.getGrid() )
    
    def getAxisIds( self, var ):
        if not hasattr( var, "coordinates" ):
            return None
        axis_ids = var.coordinates.strip().split(' ')  
        try: 
            axis_ids[0].lower().index('lat') 
            return [ axis_ids[1], axis_ids[0] ]  
        except:
            return axis_ids

    def getLatLon( self, grid_coords, **args ):
        data_file = self.df
        grid_file = self.gf
#         if grid_file:
#             lat = grid_file['lat']
#             lon = grid_file['lon']
#             if PlotType.validCoords( lat, lon ): 
#                 return  self.processCoordinates( lat, lon )
        Var = self.var        
        axis_ids = self.getAxisIds( Var )
        lat = None
        lon = None
        if axis_ids:
            try:
                if grid_file:   
                    lon = grid_file( axis_ids[0], squeeze=1 )
                    lat = grid_file( axis_ids[1], squeeze=1 )  
                elif data_file:
                    lon = data_file( axis_ids[0], squeeze=1 )
                    lat = data_file( axis_ids[1], squeeze=1 )  
            except cdms2.error.CDMSError:
                print>>sys.stderr, "Can't find lat/lon coordinate variables in file(s)."
                return None, None
            if PlotType.validCoords( lat, lon ): 
                return  self.processCoordinates( lat, lon )
        elif hasattr( Var, "stagger" ):
            stagger = Var.stagger.strip()
            if data_file <> None:
                lat = data_file( "XLAT_%s" % stagger, squeeze=1 )  
                lon = data_file( "XLONG_%s" % stagger, squeeze=1 )
                if PlotType.validCoords( lat, lon ): 
                    return  self.processCoordinates( lat, lon )

        lat = Var.getLatitude()  
        lon = Var.getLongitude()
        if PlotType.validCoords( lat, lon ): 
            return  self.processCoordinates( lat.getValue(), lon.getValue() )
        
        lon_coord = grid_coords[0] 
        lon = data_file( lon_coord, squeeze=1 ) if lon_coord else None
        lat_coord = grid_coords[1] 
        lat = data_file( lat_coord, squeeze=1 )  if lat_coord else None 
        if PlotType.validCoords( lat, lon ): 
            return  self.processCoordinates( lat, lon )  
        
        lon_coord_names = [ 'east_west', 'west_east']
        lat_coord_names = [ 'north_south', 'south_north' ]
        for axis_spec in Var.getDomain():
            for aname in lon_coord_names:
                if axis_spec[0].id.lower().find( aname ) <> -1:
                    lon = axis_spec[0]
            for aname in lat_coord_names:
                if axis_spec[0].id.lower().find( aname ) <> -1:
                    lat = axis_spec[0]
        if PlotType.validCoords( lat, lon ): 
            return  self.processCoordinates( lat.getValue(), lon.getValue() )
        
        axis_ids = []
        longitude_names = [ 'longitude', 'column longitude' ]
        latitude_names = [ 'latitude', 'column latitude' ]
        for axis_spec in Var.getDomain():
            if not ( axis_spec[0].isLevel() or axis_spec[0].isTime() ):
                axis_ids.append( axis_spec[0].id ) 
        for dset in [ data_file, grid_file ]:
            if dset:
                for cvar_name in dset.variables:
                    cvar = dset[ cvar_name ]
                    cvar_axis_ids = cvar.getAxisIds()
                    if ( len( cvar_axis_ids ) == 1 ) and ( cvar_axis_ids[0] in axis_ids ):
                        if hasattr( cvar, 'long_name' ):
                            if ( cvar.long_name.lower() in longitude_names ):
                                lon = dset( cvar.id, squeeze=1 )   
                            elif ( cvar.long_name.lower() in latitude_names ):
                                lat = dset( cvar.id, squeeze=1 )   
                        if hasattr( cvar, 'standard_name' ):
                            if ( cvar.standard_name.lower() in longitude_names ):
                                lon = dset( cvar.id, squeeze=1 )   
                            elif ( cvar.standard_name.lower() in latitude_names ):
                                lat = dset( cvar.id, squeeze=1 )   
        if PlotType.validCoords( lat, lon ): 
            return  self.processCoordinates( lat, lon ) 
        print>>sys.stdout, "Error, Can't find grid axes!"  
        return None, None
    
    def setDataSlice(self, istart, **args ):
        self.istart = istart
        self.istep = args.get( 'istep', -1 )
        self.max_points = args.get( 'max_points', -1 )
        
    def getLevel(self, var ):
        lev_aliases =  [ "isobaric", "bottom_top", "layers", "interfaces" ]
        lev = var.getLevel()
        if lev == None:
            for axis_spec in var.getDomain():
                axis = axis_spec[0]
                grid_lev = None
                if self.gf:
                    try:
                        grid_lev = self.gf[ axis.id ]
                        if grid_lev.isLevel():
                            axis.designateLevel()
                            return grid_lev if ( grid_lev.shape[0] == axis.shape[0] ) else axis
                    except: pass
                if axis.id in lev_aliases:
                    axis.designateLevel()
                    return grid_lev if ( grid_lev <> None ) else axis
            for axis_spec in var.getDomain():
                axis = axis_spec[0]
                if axis.id.find('level') <> -1:
                    axis.designateLevel()
                    return axis
        return lev

    def stepTime( self, **args ):
        process = args.get( 'process', True )
        update_points = args.get( 'update_points', True )
        self.iTimeStep = self.iTimeStep + 1
        print " PC[%d/%d]: stepTime[%d]: %s  " % ( self.istart, self.istep, self.iTimeStep, str( process ) )
        if self.iTimeStep >= self.time.shape[0]:
            self.iTimeStep = 0
        grid_var_name = self.var.id
        if process:
            var_data = self.var_data_cache.get( self.iTimeStep, None ) 
            if id(var_data) == id(None):
                var_data = self.getDataBlock(self.var)  
                self.var_data_cache[ self.iTimeStep ] = var_data
            self.point_data_arrays[ grid_var_name ] = var_data.data
            self.vrange[grid_var_name] = ( var_data.min(), var_data.max() ) 
        return process
    
    def getProcessedVariable( self, var_proc_op = None ):
        var = self.df[ self.grid_vars[0] ] if ( type( self.grid_vars[0] ) == str ) else self.grid_vars[0]           
        self.point_layout = self.getPointsLayout( var )
        if isNone( var ):
            print>>sys.stderr, "Error, can't find variable '%s' in data file." % ( self.grid_vars[0] )
            return None
        if self.roi <> None:
            if ( self.point_layout == PlotType.Grid ):
                var = var.subRegion( longitude=(self.roi[0],self.roi[2]), latitude=(self.roi[1],self.roi[3]) )
#             else:
#                 var = self.subsetUnstructuredVar( var, self.roi )
#         if var_proc_op == "anomaly_t":
#             var = cdutil.averager( var, axis='time' )
        return var
    
#     def subsetUnstructuredVar(self, var, roi ):
#         return var   # Subset later

    def getMetadata( self ):
        return self.metadata

    def initialize( self, args, **cfg_args ): 
        self.configure( **cfg_args )
        ( grid_file, data_file, interface, grd_vars, grd_coords, var_proc_op, ROI, subSpace ) = args
        self.interface = interface
        self.roi = ROI
        self.gf = cdms2.open( grid_file ) if grid_file else None
        self.df = cdms2.open( data_file ) if data_file else None         
        self.grid_vars = grd_vars if ( grd_vars <> None ) else self.df.variables[0]
        self.grid_coords = grd_coords
        self.initPoints( var_proc_op )
        
    def initPoints( self, var_proc_op=None ):
        self.var = self.getProcessedVariable( var_proc_op )
        varname = self.var.id
        self.grid = self.var.getGrid()
        self.lev = self.getLevel(self.var)
        lon, lat = self.getLatLon( self.grid_coords )  
        if not ( isNone(lat) or isNone(lon) ): 
            self.vars[ 'lat' ] = lat
            self.metadata[ 'lat' ] = ( getattr( lat, 'long_name', 'Latitude' ), getattr( lat, 'units', None ), self.axis_bounds.get( 'y', None ) )  
            self.vars[ 'lon' ] = lon 
            self.metadata[ 'lon' ] = ( getattr( lon, 'long_name', 'Longitude' ), getattr( lon, 'units', None ), self.axis_bounds.get( 'x', None ) )  
            self.time = self.var.getTime()
            z_scale = 0.5
            self.missing_value = self.var.attributes.get( 'missing_value', None )
            if self.lev == None:
                domain = self.var.getDomain()
                for axis in domain:
                    if PlotType.isLevelAxis( axis[0].id.lower() ):
                        self.lev = axis[0]
                        break 
            if self.lev <> None: 
                self.vars[ 'lev' ] = self.lev
            self.computePoints() 
            self.point_data_arrays[ 'lon' ] = self.point_data_arrays['x']                                
            self.point_data_arrays[ 'lat' ] = self.point_data_arrays['y']                            
            self.setPointHeights( height_var=self.grid_coords[3], z_scale=z_scale )
            if self.lev <> None: 
                self.metadata[ 'lev' ] = ( self.lev.__dict__.get('long_name',self.lev.id), self.lev.units, self.axis_bounds.get( 'z', None ) ) 
                self.point_data_arrays[ 'lev' ] = self.point_data_arrays['z'] 

            self.vars[ varname ] = self.var
            var_data = self.getDataBlock( self.var )
            if not isNone( var_data ):  
                self.point_data_arrays[ varname ] = var_data
                vrng = ( var_data.min(), var_data.max() )
                self.vrange[ varname ] = vrng 
#                self.var_data_cache[ self.iTimeStep ] = var_data
                var_long_name = getVarAttribute( self.var, [ 'long_name', 'name_in_file', 'id' ] )             
                var_units = getVarAttribute( self.var, [ 'units' ] ) 
                self.metadata[ varname ] = ( var_long_name, var_units, vrng )
        
    def getPoints(self):
        point_comps = [ self.point_data_arrays[comp].flat for comp in [ 'x', 'y', 'z'] ]
        return numpy.dstack( point_comps ).flatten()

    def getPointIndices(self):
        return self.selected_index_array
    
    def getPointHeights(self):
        return self.point_data_arrays['z'] 

    def getVarData(self, var_name=None):
        if not var_name: var_name = self.var.id
        return  self.point_data_arrays.get( var_name, None )

    def getVarDataRange(self, var_name=None ):
        if not var_name: var_name = self.var.id
        return self.vrange.get( var_name, None )

    def getThresholdedRange(self, var_name=None ):
        if not var_name: var_name = self.var.id
        return self.thresholded_range.get( var_name, None )
    
    def getThresholdTargetType(self):
        return self.thresholdTargetType
        
    def getNLevels(self):
        return len( self.lev ) if self.lev else 1
    
    def computeThresholdRange( self, args ):
#        print " computeThresholdRange: ", str( args )
        try:
            ( threshold_target, rmin, rmax, normalized ) = args
        except ValueError:
            print>>sys.stderr, "Value Error Unpacking thresholding data: %s " % str( args )
            return None, None, None
        except Exception:
            print>>sys.stderr, "Exception Unpacking thresholding data: %s " % str( args )
            return None, None, None
        vmin = None
        var_data_id = self.var.id if ( threshold_target == 'vardata' ) else threshold_target
        var_data = self.point_data_arrays.get( var_data_id, None) 
        if not isNone(var_data):           
            arange = self.axis_bounds.get( threshold_target )
            try:
                if arange:
                    dv = arange[1] - arange[0]
                    vmin = arange[0] + rmin * dv
                    vmax = arange[0] + rmax * dv  
                elif ( threshold_target == 'vardata' ) or ( threshold_target in self.vars.keys() ):
                    vrng = self.vrange[ var_data_id ]
                    if normalized:
                        dv = vrng[1] - vrng[0]
                        try:
                            vmin = vrng[0] + rmin * dv
                            vmax = vrng[0] + rmax * dv
                        except TypeError, err:
                            pass
                    else:
                        vmin = rmin
                        vmax = rmax                  
                if vmin <> None:
                    if ( threshold_target == 'z' ):
                        nLev = len( self.lev )
                        rave = (rmin + rmax)/2
                        iLev = int(  nLev * rave  )  if self.levelsAreAscending() else int(  nLev * (1.0-rave)  ) 
                        lev_val = self.lev[ iLev ]
                        self.thresholded_range[var_data_id] = [ lev_val, lev_val ]
    #                    print "Z threshold Range: %d %f " % ( iLev, lev_val )
                    else:
                        self.thresholded_range[var_data_id] = [ vmin, vmax ]
                    return var_data.flatten(), vmin, vmax
            except TypeError:
                print>>sys.stderr, "Range Error Computing Threshold: ", str(arange )
        return None, None, None
                    
    def execute( self, args, **kwargs ): 
        op = args[0] 
        if op == 'indices': 
            threshold_mask = None
#            print "Processing computeThresholdRange: %s " % str( args )
            for var_op in args[1:]:  
                var_data, vmin, vmax = self.computeThresholdRange( var_op )               
                if not isNone(var_data):
                    var_mask = numpy.logical_and( numpy.greater_equal( var_data, vmin ), numpy.less_equal( var_data, vmax ) )  
#                    print "MultiVarPointCollection.execute: %s, mask range = %s  " % ( str( args ), str( (vmin, vmax) ) ); sys.stdout.flush()
                    if isNone(threshold_mask):                       
                        self.thresholdTargetType = 'coords' if var_op[0] in [ 'lat', 'lon', 'lev', 'x', 'y', 'z' ] else 'vardata' 
                        threshold_mask = var_mask
                    else:
                        threshold_mask = numpy.logical_and( threshold_mask, var_mask )
            if isNone( threshold_mask ):
                print>>sys.stderr, "Thresholding failed for spec: ", str( args )
                return None, None
            else:
                index_array = numpy.arange( 0, len(threshold_mask) )
                self.selected_index_array = index_array[ threshold_mask ]  
                return vmin, vmax   
        elif op == 'points': 
#            print " subproc: Process points request, args = %s " % str( args ); sys.stdout.flush()
            if args[2] <> None:
                self.setPointHeights( height_var=args[1], z_scale=args[2] )  
        elif op == 'ROI': 
            ROI = args[1]
            self.setROI(ROI)            
        elif op == 'timestep': 
            self.stepTime( **kwargs )  

