'''
Created on Sep 18, 2013

@author: tpmaxwel
'''

import sys, os
import numpy
from cdms2.error import CDMSError
import vtk,  time,  math, threading
from vtk.util import numpy_support
from MultiVarPointCollection import MultiVarPointCollection, PlotType, isNone
from PointCollection import PointCollection
from ConfigurationFunctions import SIGNAL
from multiprocessing import Process, Queue

class ScalarRangeType:         
    Full = 0
    Thresholded = 1

class PCProc:
    Subset = 0
    ZScaling = 1
    Timestep = 2

class ExecutionDataPacket:
    NONE = -1
    POINTS = 0
    INDICES = 1
    VARDATA = 2
    HEIGHTS = 3
    
    def __init__( self, msg_type, node_index, data_object  ):
        self.type = msg_type
        self.data = data_object
        self.node_index = node_index
        self.metadata = {}
#        print "ExecutionDataPacket[%d]: type=%s, shape=%s" % ( node_index, self.getTypeStr(msg_type), str(data_object.shape) )
    
    @classmethod    
    def getTypeStr( cls, type ):
        if ( type == cls.NONE ): return "NONE"
        if ( type == cls.POINTS ): return "POINTS"
        if ( type == cls.INDICES ): return "INDICES"
        if ( type == cls.VARDATA ): return "VARDATA"
        if ( type == cls.HEIGHTS ): return "HEIGHTS"

    def printLogMessage(self, msg_str ):
        print " DataPacket %d: %s" % ( self.node_index, msg_str )
        sys.stdout.flush()      
        
    def __len__(self):
        return len(self.metadata)

    def __getitem__(self, key):
        return self.metadata.get( key, None )

    def __setitem__(self, key, value):
        self.metadata[key] = value

    def __delitem__(self, key):
        del self.values[key]    

class PointCollectionExecutionTarget:

    def __init__( self, collection_index, ncollections, init_args=None, **cfg_args ):
        self.point_collection = MultiVarPointCollection() 
        self.point_collection.setDataSlice( collection_index, istep=ncollections )
        self.collection_index = collection_index
        self.ncollections = ncollections
        self.init_args = init_args
        self.cfg_args = cfg_args

    def printLogMessage(self, msg ):
        print " PointCollectionExecutionTarget %d: %s" % ( self.collection_index, str(msg) )
        sys.stdout.flush()      

    def __call__( self, args_queue, result_queue ):
        self.results = result_queue
        self.initialize()
        while True:
            args = list( args_queue.get( True ) )
            self.execute( args )
                
    def initialize( self ):
        self.point_collection.initialize( self.init_args, **self.cfg_args )
        self.point_collection.setDataSlice( self.collection_index, istep=self.ncollections )
        self.results.put( self.packPointsData() )
        self.results.put( self.packVarData() )
                       
    def execute( self, args ):
        try:
            self.point_collection.execute( args )
            if args[0] == 'indices':
                data_packet = self.packIndexData()
            elif args[0] == 'points':
                data_packet = self.packPointHeightsData()
            elif args[0] == 'timestep':
                data_packet = self.packVarData()
            elif args[0] == 'ROI':
                data_packet = self.packPointsData()
            data_packet[ 'args' ] = args
            
            self.results.put( data_packet )
        except Exception, err:
            print>>sys.stderr, "Error executing PointCollectionExecutionTarget: ", str( err )

    def packVarData(self):
#        print "Pack VARDATA"; sys.stdout.flush()
        vardata = self.point_collection.getVarData() 
        data_packet = ExecutionDataPacket( ExecutionDataPacket.VARDATA, self.collection_index, vardata.data )
        data_packet[ 'fill_value' ] = vardata.fill_value
        data_packet[ 'vrange' ] = self.point_collection.getVarDataRange() 
        data_packet[ 'grid' ] = self.point_collection.getGridType()  
        data_packet[ 'nlevels' ] = self.point_collection.getNLevels()
        data_packet[ 'bounds' ] = self.point_collection.getBounds()
        return data_packet

    def packPointsData( self ):
#        print "Pack POINTS"; sys.stdout.flush()
        data_packet = ExecutionDataPacket( ExecutionDataPacket.POINTS, self.collection_index, self.point_collection.getPoints() )
        return data_packet

    def packPointHeightsData( self ):
#        print " ExecutionTarget-%d: packPointHeightsData" % ( self.collection_index )
        data_packet = ExecutionDataPacket( ExecutionDataPacket.HEIGHTS, self.collection_index, self.point_collection.getPointHeights() )
        data_packet[ 'bounds' ] = self.point_collection.getBounds()
        return data_packet

    def packIndexData( self ):
        data_packet = ExecutionDataPacket( ExecutionDataPacket.INDICES, self.collection_index, self.point_collection.getPointIndices() )
        target = self.point_collection.getThresholdTargetType()
#        range_type = 'trange' if ( target == "vardata" ) else "crange"
#        data_packet[ target ] = self.point_collection.getThresholdTargetType() 
        data_packet[ 'target' ] = target
        data_packet[ 'trange' ] = self.point_collection.getThresholdedRange()
        return data_packet

class vtkPointCloud():

    shperical_to_xyz_trans = vtk.vtkSphericalTransform()
    radian_scaling = math.pi / 180.0 

    def __init__( self, pcIndex=0, nPartitions=1 ):
        self.nPartitions = nPartitions
        self.polydata = None
        self.vardata = None
        self.vrange = None
        self.np_index_seq = None
        self.np_cell_data = None
        self.points = None
        self.pcIndex = pcIndex
        self.earth_radius = 100.0
        self.spherical_scaling = 0.4
        self.vtk_planar_points = None
        self.vtk_spherical_points = None
        self.np_points_data = None
        self.topo = PlotType.Planar
        self.grid = None
#        self.threshold_target = "vardata"
        self.current_scalar_range = None
        self.nlevels  = None
        self.current_subset_specs = None
        self.updated_subset_specs = None
        self.mapper = vtk.vtkPolyDataMapper()
        self.mapper.SetScalarModeToUsePointData()
        self.mapper.SetColorModeToMapScalars()
        self.actor = vtk.vtkActor()
        self.actor.SetMapper( self.mapper )
      
    def getPoint( self, iPt ):
        try:
            dval = self.vardata[ iPt ]
            pt = self.vtk_planar_points.GetPoint( iPt ) 
            self.printLogMessage( " getPoint[%d/%d]: dval=%s, pt=%s " % ( iPt, self.vardata.shape[0], str(dval), str(pt) ) ) 
        except CDMSError, err:
            print>>sys.stderr, "Pick Error for point %d: %s" % ( iPt, str(err) )
            print>>sys.stderr, "Vardata(%s) shape: %s " % ( self.vardata.__class__.__name__, str( self.vardata.shape ) )
            return None, None
        return pt, dval
        
    def printLogMessage(self, msg_str, **args ):
        error = args.get( "error", False )
        if error:   print>>sys.stderr, " Proxy Node %d Error: %s" % ( self.pcIndex, msg_str )
        else:       print " Proxy Node %d: %s" % ( self.pcIndex, msg_str )
        sys.stdout.flush() 
        
    def getNLevels(self): 
#        if self.nlevels == None: 
#            print>>sys.stderr, " Undefined nlevels in getNLevels, proc %d " % self.pcIndex
        return self.nlevels    
    
    def getGrid(self):
        return self.grid
    
    def hasResultWaiting(self):
        return False
    
    def getThresholdingRanges(self): 
        return self.current_subset_specs      
#        return self.trange if ( self.threshold_target == "vardata" ) else self.crange
    
    def getValueRange( self, var_name=None, range_type = ScalarRangeType.Full ):
        return self.vrange if ( range_type == ScalarRangeType.Full ) else self.trange
   
    def generateSubset(self, **args ):
        pass
    
    def refresh( self, force = False ):
        if force or (self.current_subset_specs and (self.current_subset_specs <> self.updated_subset_specs)):
            self.generateSubset()
            self.updated_subset_specs = self.current_subset_specs
    
    def getData( self, dtype ):
        if dtype == ExecutionDataPacket.VARDATA:
            return self.vardata
        elif dtype == ExecutionDataPacket.INDICES:
            return self.np_index_seq 
        elif dtype == ExecutionDataPacket.POINTS:
            return self.np_points_data 
        elif dtype == ExecutionDataPacket.HEIGHTS:
            return self.np_points_data 
        
    def getCellData(self):
        return self.np_cell_data
   
    def updateVertices( self, **args ): 
        self.vertices = vtk.vtkCellArray()  
        if isNone(self.np_index_seq):
            wait = args.get( 'wait', False )  
            if wait: self.waitForData( ExecutionDataPacket.INDICES )
            else: return
        cell_sizes   = numpy.ones_like( self.np_index_seq )
        self.np_cell_data = numpy.dstack( ( cell_sizes, self.np_index_seq ) ).flatten()         
        self.vtk_cell_data = numpy_support.numpy_to_vtkIdTypeArray( self.np_cell_data ) 
        self.vertices.SetCells( cell_sizes.size, self.vtk_cell_data )     
        self.polydata.SetVerts(self.vertices)
#        self.polydata.Modified()
#        self.mapper.Modified()
#        self.actor.Modified()
#        self.actor.SetVisibility( True  )
        
    def getPolydata(self):
        return self.polydata
        
    def setNormalizedScalarRange( self, normalized_scalar_range ):
        self.setScalarRange( self.getScaledRange( normalized_scalar_range ) )
        return self.current_scalar_range

    def setScalarRange( self, scalar_range=None ):
        if scalar_range: self.current_scalar_range = scalar_range
        self.mapper.SetScalarRange( self.current_scalar_range[0], self.current_scalar_range[1] )
#        print " ------------------------->>>>>>>>>>>>>>>>>>>> PointCloud: Set Scalar Range: %s " % str( self.current_scalar_range ) 
        self.mapper.Modified()
        self.actor.Modified()
        self.actor.SetVisibility( True  )
       
    def getScalarRange( self ):
        return self.current_scalar_range
    
    def getScaledRange( self, srange ):
        dv = self.vrange[1] - self.vrange[0]
        vmin = self.vrange[0] + srange[0] * dv
        vmax = self.vrange[0] + srange[1] * dv
        return ( vmin, vmax )
                                             
    def updateScalars( self, **args ):
        if isNone(self.vardata):
            wait = args.get( 'wait', True ) 
            if wait: self.waitForData( ExecutionDataPacket.VARDATA )
            else: return
        vtk_color_data = numpy_support.numpy_to_vtk( self.vardata ) 
        vtk_color_data.SetName( 'vardata' )       
        self.polydata.GetPointData().SetScalars( vtk_color_data )
        self.polydata.Modified()
        self.mapper.Modified()
        self.actor.Modified()
        
    def initPoints( self, **args ):
        if isNone(self.np_points_data):
            wait = args.get( 'wait', True ) 
            if wait: self.waitForData( ExecutionDataPacket.POINTS )
            else: return
        vtk_points_data = numpy_support.numpy_to_vtk( self.np_points_data )    
        vtk_points_data.SetNumberOfComponents( 3 )
        vtk_points_data.SetNumberOfTuples( int( self.np_points_data.size / 3 ) )     
        self.vtk_planar_points = vtk.vtkPoints()
        self.vtk_planar_points.SetData( vtk_points_data )
        self.createPolydata( **args )

    def setPointHeights( self, ptheights ):
        try:
            if self.topo == PlotType.Planar:   
                self.np_points_data[2::3] =  ptheights
                vtk_points_data = numpy_support.numpy_to_vtk( self.np_points_data ) 
                vtk_points_data.SetNumberOfComponents( 3 )
                vtk_points_data.SetNumberOfTuples( len( self.np_points_data ) / 3 ) 
                self.vtk_planar_points.SetData( vtk_points_data )
                self.polydata.SetPoints( self.vtk_planar_points )  
                self.vtk_planar_points.Modified()
            elif self.topo == PlotType.Spherical:
                self.np_sp_grid_data[0::3] =  self.spherical_scaling * ptheights + self.earth_radius
                vtk_sp_grid_data = numpy_support.numpy_to_vtk( self.np_sp_grid_data ) 
                size = vtk_sp_grid_data.GetSize()                    
                vtk_sp_grid_data.SetNumberOfComponents( 3 )
                vtk_sp_grid_data.SetNumberOfTuples( size/3 )   
                vtk_sp_grid_points = vtk.vtkPoints()
                vtk_sp_grid_points.SetData( vtk_sp_grid_data )
                self.vtk_spherical_points = vtk.vtkPoints()
                self.shperical_to_xyz_trans.TransformPoints( vtk_sp_grid_points, self.vtk_spherical_points ) 
#                pt0 = self.vtk_spherical_points.GetPoint(0)
    #            print "VTK Set point Heights, samples: %s %s %s " % ( str( ptheights[0] ), str( self.np_sp_grid_data[0] ), str( pt0 ) )
                self.polydata.SetPoints( self.vtk_spherical_points ) 
                self.vtk_spherical_points.Modified()
            self.polydata.Modified()
        except Exception, err:
            self.printLogMessage( "Processing point heights: %s " % str( err ), error=True )
        
    def createPolydata( self, **args  ):
        if self.polydata == None:
            self.polydata = vtk.vtkPolyData()
            vtk_pts = self.getPoints()
            self.polydata.SetPoints( vtk_pts )                         
            self.initializePointsActor( self.polydata, **args )

    def computeSphericalPoints( self, **args ):
        lon_data = self.np_points_data[0::3]
        lat_data = self.np_points_data[1::3]
        z_data = self.np_points_data[2::3]
        radian_scaling = math.pi / 180.0 
        theta =  ( 90.0 - lat_data ) * radian_scaling
        phi = lon_data * radian_scaling
        
        r = z_data * self.spherical_scaling + self.earth_radius
        self.np_sp_grid_data = numpy.dstack( ( r, theta, phi ) ).flatten()
        vtk_sp_grid_data = numpy_support.numpy_to_vtk( self.np_sp_grid_data ) 

#         if self.grid == PlotType.List:
# #             r = numpy.empty( lon_data.shape, lon_data.dtype )      
# #             r.fill(  self.earth_radius )
#             r = z_data * self.spherical_scaling + self.earth_radius
#             self.np_sp_grid_data = numpy.dstack( ( r, theta, phi ) ).flatten()
#             vtk_sp_grid_data = numpy_support.numpy_to_vtk( self.np_sp_grid_data ) 
#         elif self.grid == PlotType.Grid:
#             thetaB = theta.reshape( [ theta.shape[0], 1 ] )  
#             phiB = phi.reshape( [ 1, phi.shape[0] ] )
#             grid_data = numpy.array( [ ( self.earth_radius, t, p ) for (t,p) in numpy.broadcast(thetaB,phiB) ] )
#             self.np_sp_grid_data = grid_data.flatten() 
#             vtk_sp_grid_data = numpy_support.numpy_to_vtk( self.np_sp_grid_data ) 
#         else:
#             print>>sys.stderr, "Unrecognized grid type: %s " % str( self.grid )
#             return        
        size = vtk_sp_grid_data.GetSize()                    
        vtk_sp_grid_data.SetNumberOfComponents( 3 )
        vtk_sp_grid_data.SetNumberOfTuples( size/3 )   
        vtk_sp_grid_points = vtk.vtkPoints()
        vtk_sp_grid_points.SetData( vtk_sp_grid_data )
        self.vtk_spherical_points = vtk.vtkPoints()
        self.shperical_to_xyz_trans.TransformPoints( vtk_sp_grid_points, self.vtk_spherical_points ) 
                
    def initializePointsActor( self, polydata, **args ):
        lut = args.get( 'lut', None )
        if lut == None: lut = self.create_LUT() 
        if vtk.VTK_MAJOR_VERSION <= 5:  self.mapper.SetInput(self.polydata)
        else:                           self.mapper.SetInputData(self.polydata)        
        if lut:  self.mapper.SetLookupTable( lut )                
#        if self.vrange:
#            self.mapper.SetScalarRange( self.vrange[0], self.vrange[1] ) 
#            self.printLogMessage( " init scalar range %s " % str(self.vrange) )    

    def getNumberOfPoints(self): 
        return len( self.np_points_data ) / 3             
    
    def getPoints( self, **args ):
        if self.topo == PlotType.Spherical:
            if not self.vtk_spherical_points:
                self.refresh()
                self.computeSphericalPoints()
            return self.vtk_spherical_points
        if self.topo == PlotType.Planar:
            if not self.vtk_planar_points: 
                self.initPoints( **args )
            return self.vtk_planar_points
        
    def updatePoints( self, clear=False ):
        if clear:
            self.np_points_data = self.point_collection.getPoints()
            self.vrange = self.point_collection.getVarDataRange()
            self.vtk_spherical_points = None
            self.vtk_planar_points = None
        self.polydata.SetPoints( self.getPoints() ) 

    @classmethod    
    def getXYZPoint( cls, lon, lat, r = None ):
        theta =  ( 90.0 - lat ) * cls.radian_scaling
        phi = lon * cls.radian_scaling
        spherical_coords = ( r, theta, phi )
        return cls.shperical_to_xyz_trans.TransformDoublePoint( *spherical_coords )

    def setTopo( self, topo, **args ):
        if topo <> self.topo:
            self.topo = topo
            self.clearClipping()
#            if self.actor.GetVisibility():
            pts = self.getPoints( **args )
            self.polydata.SetPoints( pts ) 
            return pts
        return None 
        
    def setVisiblity(self, visibleLevelIndex ):
        isVisible = ( visibleLevelIndex < 0 ) or ( visibleLevelIndex == self.iLevel )
        if isVisible: 
            self.updatePoints()
        self.actor.SetVisibility( isVisible  )
        return isVisible
    
    def isVisible(self):
        return self.actor.GetVisibility()
    
    def hide(self):
        self.actor.VisibilityOff()

    def show(self):
        if not self.actor.GetVisibility():
            self.actor.VisibilityOn()
       
    def getBounds( self, **args ):
        topo = args.get( 'topo', self.topo )
        lev = args.get( 'lev', None )
        if topo == PlotType.Spherical:
            return [ -self.earth_radius, self.earth_radius, -self.earth_radius, self.earth_radius, -self.earth_radius, self.earth_radius ]
        else:
            b = list( self.grid_bounds )
#            if lev:
#                lev_bounds = ( lev[0], lev[-1] )
#                b[4] = lev_bounds[0] if ( lev_bounds[0] < lev_bounds[1] ) else lev_bounds[1]
#                b[5] = lev_bounds[1] if ( lev_bounds[0] < lev_bounds[1] ) else lev_bounds[0]
#            elif ( b[4] == b[5] ):
#                b[4] = b[4] - 100.0
#                b[5] = b[5] + 100.0
            return b

    def getAxisBounds( self, **args ):
        return list( self.grid_bounds )
#         topo = args.get( 'topo', self.topo )
#         lev = args.get( 'lev', None )
#         if topo == PlotType.Spherical:
#             return [ 0.0, 360.0, -90.0, 90.0, -self.earth_radius, self.earth_radius ]
#         else:
#             b = list( self.grid_bounds )
# #            if lev:
# #                lev_bounds = ( lev[0], lev[-1] )
# #                b[4] = lev_bounds[0] if ( lev_bounds[0] < lev_bounds[1] ) else lev_bounds[1]
# #                b[5] = lev_bounds[1] if ( lev_bounds[0] < lev_bounds[1] ) else lev_bounds[0]
# #            elif ( b[4] == b[5] ):
# #                b[4] = b[4] - 100.0
# #                b[5] = b[5] + 100.0
#             return b
                
    def setClipping( self, clippingPlanes ):
        self.mapper.SetClippingPlanes( clippingPlanes )
        
    def clearClipping( self ):
        self.mapper.RemoveAllClippingPlanes()    

    def setPointSize( self, point_size ):
        if point_size <> None:
            try:
                self.actor.GetProperty().SetPointSize( point_size )
            except TypeError:
                print>>sys.stderr, "Error setting point size: value = %s " % str( point_size )

    def getPointSize( self ):
        return self.actor.GetProperty().GetPointSize()
        
    def getPointValue( self, iPt ):
        return self.var_data[ iPt ]

    def create_LUT( self, **args ):
        lut = vtk.vtkLookupTable()
        lut_type = args.get( 'type', "blue-red" )
        invert = args.get( 'invert', False )
        number_of_colors = args.get( 'number_of_colors', 256 )
        alpha_range = 1.0, 1.0
        
        if lut_type=="blue-red":
            if invert:  hue_range = 0.0, 0.6667
            else:       hue_range = 0.6667, 0.0
            saturation_range = 1.0, 1.0
            value_range = 1.0, 1.0
         
        lut.SetHueRange( hue_range )
        lut.SetSaturationRange( saturation_range )
        lut.SetValueRange( value_range )
        lut.SetAlphaRange( alpha_range )
        lut.SetNumberOfTableValues( number_of_colors )
        lut.SetRampToSQRT()            
        lut.Modified()
        lut.ForceBuild()
        return lut   

class vtkSubProcPointCloud( vtkPointCloud ):

    def __init__( self, pcIndex=0, nPartitions=1 ):
        vtkPointCloud.__init__( self, pcIndex, nPartitions )
        self.arg_queue = Queue() # JoinableQueue() 
        self.result_queue = Queue() # JoinableQueue()
        self.parameter_cache = {}
            
    def runProcess(self, procType, **args):
        if   procType == PCProc.Subset:    self.generateSubset( **args )
        elif procType == PCProc.ZScaling:  self.generateZScaling( **args )
        elif procType == PCProc.Timestep:  self.stepTime( **args )

    def setROI( self, ROI ): 
        self.clearQueues()
        op_specs = [ 'ROI', ROI ]
        self.arg_queue.put( op_specs,  False ) 
        
    def generateSubset(self, **args ):
        self.current_subset_specs = args.get( 'spec', self.current_subset_specs )
#        print " vtkSubProcPointCloud[%d]: current_subset_specs: %s (%s) " % ( self.pcIndex, self.current_subset_specs, str(args) )
        process = args.get( 'process', True )
        if process:
            self.clearQueues()
#            first_spec = self.current_subset_specs.values()[0]
#            self.threshold_target = first_spec[0]
            self.np_index_seq = None
#             if self.pcIndex == 1: 
#            self.printLogMessage( " vtkSubProcPointCloud --->> Generate subset: %s " % str(self.current_subset_specs) )
            op_specs = self.current_subset_specs.values()
            op_specs.insert( 0, 'indices' )
            self.arg_queue.put( op_specs,  False ) 

    def generateZScaling(self, **args ):
        z_subset_spec = args.get('spec', None )
        zscale_value = z_subset_spec[1]
        cached_zscale_value = self.parameter_cache.get( 'zscale', None ) 
        if cached_zscale_value <> zscale_value:
            self.clearQueues()
            op_specs = [ 'points' ] + list(z_subset_spec)
#             print " Generate Z Scaling [P-%d]: %s " % ( self.pcIndex, str( args ) )
            self.arg_queue.put( op_specs,  False )
            self.parameter_cache['zscale'] = zscale_value

    def stepTime( self, **args ):
        op_specs = [ 'timestep' ]
        self.arg_queue.put( op_specs,  False ) 
        
    def getResults( self, block = False ):
        try:
            result = self.result_queue.get( block )
        except Exception:
            return False
        if result.type == ExecutionDataPacket.VARDATA:
#            print "Got VARDATA"
            self.vardata = result.data 
            self.vrange = result['vrange']
            self.grid = result['grid']
            self.nlevels = result['nlevels']
            self.grid_bounds = result['bounds']
#            print "getResults: Set grid bounds: %s " % str( self.grid_bounds )
            self.current_scalar_range = self.vrange
#            self.printLogMessage( " update vrange %s " % str(self.vrange) )      
        elif result.type == ExecutionDataPacket.INDICES:
#            print "Got INDICES"
            self.np_index_seq = result.data 
#            self.trange = result['trange']
#            self.threshold_target = result['target']
#             if self.pcIndex == 1: 
#                 self.printLogMessage(  " vtkSubProcPointCloud --->> Get Results, Args: %s " % str(result['args']) )
        elif result.type == ExecutionDataPacket.POINTS:
#            print "Got POINTS"
            self.np_points_data = result.data
        return True
    

    def processResults( self ):
        try:
            result = self.result_queue.get( False )
        except Exception:
            return ExecutionDataPacket.NONE
        if result.type == ExecutionDataPacket.VARDATA:
            self.vardata = result.data 
            self.vrange = result['vrange']
            self.grid = result['grid']
            self.nlevels = result['nlevels']
            self.grid_bounds = result['bounds']
            self.updateScalars()   
#            print " processResults[ %d ] : VARDATA" % self.pcIndex; sys.stdout.flush()
        elif result.type == ExecutionDataPacket.INDICES:
            self.np_index_seq = result.data 
#            self.threshold_target = result['target']
#            if self.threshold_target == "vardata":
#                self.trange = result['trange']
#            else:
#                self.crange = result['crange']                
#             if self.pcIndex == 1:
#                 self.printLogMessage(  " vtkSubProcPointCloud --->> Process Results, Args: %s " % str(result['args']) )
            self.updateVertices()  
#            print " processResults[ %d ] : INDICES, metadata = %s " % ( self.pcIndex, str(result.metadata)); sys.stdout.flush()
        elif result.type == ExecutionDataPacket.HEIGHTS:
#            print " processResults[ %d ] : POINTS" % self.pcIndex; sys.stdout.flush()
            self.setPointHeights( result.data )
            self.grid_bounds = result['bounds']
#            print "processResults: Set grid bounds: %s " % str( self.grid_bounds )
        return True
        
    def waitForData( self, dtype ):
#        self.printLogMessage( " waitForData type %d" % ( dtype ) )   
        while( id(self.getData( dtype ) ) == id( None ) ):
            self.getResults(True)
            time.sleep(0.05)
                                             
        
    def start_subprocess( self, init_args, **args ):
        exec_target =  PointCollectionExecutionTarget( self.pcIndex, self.nPartitions, init_args, **args ) 
        self.process = Process( target=exec_target, args=( self.arg_queue, self.result_queue ) )
        self.process.start()
        
    def clearQueues(self):
        try: 
            while True: self.arg_queue.get_nowait()
        except: pass
        try: 
            while True: self.result_queue.get_nowait()
        except: pass
               
    def terminate(self):
        self.process.terminate()
        
        
class vtkLocalPointCloud( vtkPointCloud ):

    def __init__( self, istart, **args ):
        vtkPointCloud.__init__( self )
        self.point_collection = MultiVarPointCollection()
        self.point_collection.setDataSlice( istart, **args )

    def getPointCollection( self ):
        return self.point_collection
    
    def setROI( self, roi ):
        self.point_collection.setROI( roi )
        self.initialize()
        
    def getMetadata( self ):
        return self.point_collection.getMetadata()

    def generateZScaling(self, **args ):
        z_subset_spec = args.get('spec', None )
        op_specs = [ 'points' ] + list( z_subset_spec )
        self.point_collection.execute( op_specs ) 
        self.setPointHeights( self.point_collection.getPointHeights()  )   
        self.grid_bounds = self.point_collection.getAxisBounds()
#        print "generateZScaling: Set grid bounds: %s " % str( self.grid_bounds )
        self.polydata.Modified()
        self.mapper.Modified()
        self.actor.Modified()
        self.actor.SetVisibility( True  )

    def generateSubset(self, **args ):
#        print " ++++++++++++++++++++++ vtkLocalPointCloud[%d].generateSubset: current_subset_specs: %s (%s) " % ( self.pcIndex, self.current_subset_specs, str(args) )
        self.current_subset_specs = args.get('spec', self.current_subset_specs)
#         if self.current_subset_specs[0] == 'Z3':
#             print " vtkLocalPointCloud[%d]: current_subset_specs: %s (%s) " % ( self.pcIndex, self.current_subset_specs, str(args) )
#        self.threshold_target = self.current_subset_specs[0]
        op_specs = [ spec for spec in  self.current_subset_specs.values() ]
        op_specs.insert( 0, 'indices' )
        vmin, vmax = self.point_collection.execute( op_specs )       
        self.np_index_seq = self.point_collection.selected_index_array
#        if self.threshold_target == "vardata": self.trange = ( vmin, vmax )
#        else: 
#            self.crange = ( vmin, vmax )
#            print "Set crange: ", str( self.crange )
        self.grid = self.point_collection.getGridType()
        self.current_scalar_range = self.vrange
        self.updateVertices() 
        self.updated_subset_specs = self.current_subset_specs
        
    def getNumberOfInputPoints(self): 
        return self.point_collection.getNumberOfInputPoints()
    
    def getSkipIndex(self): 
        return self.point_collection.istep
        
    def initialize(self, init_args = None, **args ):
        if init_args: self.point_collection.initialize( init_args, **args )
        else: self.point_collection.initPoints() 
        self.np_points_data = self.point_collection.getPoints()
        self.vrange = self.point_collection.getVarDataRange()
        self.initPoints( **args ) 
        self.createPolydata( **args )
        self.vardata = self.point_collection.getVarData()
        self.updateScalars() 
        self.grid_bounds = self.point_collection.getAxisBounds()
#        print "initialize: Set grid bounds: %s " % str( self.grid_bounds )
        self.nlevels = self.point_collection.getNLevels()
        self.actor.VisibilityOff()
        
    def getCenter(self):
        return ( self.point_collection.xcenter, self.point_collection.ycenter, self.point_collection.xwidth, self.point_collection.ywidth )
    
    def stepTime(self, **args): 
        if self.point_collection.stepTime( **args ):
            update_points = args.get( 'update_points', True )
            self.vardata = self.point_collection.getVarData()
            if update_points: self.generateSubset()
            self.updateScalars() 
                  
class vtkPartitionedPointCloud:
    TimerType = 10
    CheckProcQueueEventId = 10
    
    def __init__( self, nPartitions, init_args, **args  ):
        self.NewDataAvailable = SIGNAL('newDataAvailable')
        self.point_clouds = {}
        self.point_cloud_map = {}
        self.interactor = args.get( 'interactor', None )
        self.nPartitions = int( round( nPartitions ) )
        self.nActiveCollections =  self.nPartitions 
        self.current_spec = {}
        self.timerId = -1
        self.subproc_responses = 0
#        self.proc_timer = threading.Timer( 0.1, self.processProcQueue )
        for pcIndex in range( self.nPartitions ):
            pc = vtkSubProcPointCloud( pcIndex, nPartitions )
            pc.start_subprocess( init_args, **args )
            self.point_clouds[ pcIndex ] = pc
        for pc in self.point_clouds.values():
            pc.createPolydata( **args )
            pc.updateScalars()
            self.point_cloud_map[ pc.actor ] = pc
#        self.scalingTimer = self.startTimer(1000)
     
    def startCheckingProcQueues( self ):
        if self.timerId == -1:
            self.interactor.SetTimerEventId(self.CheckProcQueueEventId)
            self.interactor.SetTimerEventType( self.TimerType )
            self.timerId = self.interactor.CreateRepeatingTimer( 100 )
            
    def refresh( self, force = False ): 
        for pc in self.point_clouds.values():
            pc.refresh( force )
            
    def processTimerEvent( self, event_id ):
        if event_id == self.CheckProcQueueEventId: 
            return self.checkProcQueues()
        return False

    def setROI( self, ROI ): 
        for pc in self.point_clouds.values():
            pc.setROI( ROI )

    def stopCheckingProcQueues(self):
        if self.timerId <> -1:
            self.interactor.DestroyTimer( self.timerId )
            self.timerId = -1
            self.subproc_responses = 0
            self.clearQueues()
            
    def clearQueues(self):
        for pc_item in self.point_clouds.items():  
            pc_item[1].clearQueues()    
            
#         print "stopCheckingProcQueues: InteractionStyle = %s " % (  self.interactor.GetInteractorStyle().__class__.__name__ ) 
#         self.interactor.SetInteractorStyle( vtk.vtkInteractorStyleTrackballCamera() )
        
#     def timerEvent( self, event ):
#         if event.timerId() == self.dataQueueTimer: self.checkProcQueues()
        
    def processProcQueue(self):
#        print "---> processProcQueue: [%d] items" %  len( self.point_clouds ) 
        for pc_item in self.point_clouds.items():
            rv = pc_item[1].processResults()
#            print "---> processResults [%d]:  %s" % ( pc_item[0], str(rv) )
            if rv <> ExecutionDataPacket.NONE:
                return pc_item, rv 
        return None, None

    def terminate(self):
        for pc_item in self.point_clouds.items():
            pc_item[1].terminate()
                    
    def checkProcQueues(self):
        pc_item, rv = self.processProcQueue()
        if rv:
#            print "---> CheckProcQueues: NewDataAvailable ( %s, %s )" % ( str(pc_item[0]), str(rv) )
            self.NewDataAvailable( pc_item[0], rv )
            self.subproc_responses = self.subproc_responses + 1
            if self.subproc_responses == len( self.point_clouds ): 
                self.stopCheckingProcQueues()
            pc_item[1].show()
            return True
        return False
            
    def updateNumActiveCollections( self, ncollections_inc ):
        self.nActiveCollections = max( self.nActiveCollections + ncollections_inc, 1 )
        self.nActiveCollections = min( self.nActiveCollections, self.nPartitions )
        self.show()
        self.generateSubset()
        print " --> updateNumActiveCollections: %d " % self.nActiveCollections; sys.stdout.flush()
        
    def setResolution( self, res ):
        n_collections = int( round( self.nPartitions * res ) )
        if n_collections <> self.nActiveCollections:
            pc_base_index = self.nActiveCollections
            print " --> updateNumActiveCollections(%.2f): %d, pc_base_index: %d " % ( res, n_collections, pc_base_index ); sys.stdout.flush()
            self.nActiveCollections = n_collections
            self.generateSubset( pc_base_index = pc_base_index )
            self.hideInactives()
            
    def hasActiveCollections(self):
        return (self.nActiveCollections > 0)
            
    def clear(self, activePCIndex = -1 ):
        self.stopCheckingProcQueues() 
        for pc_item in self.point_clouds.items():
            if pc_item[0] <> activePCIndex:
                pc_item[1].hide()
                
    def show(self): 
        print "DistributedPointCollections- show()"
        for pc_item in self.point_clouds.items():
            if pc_item[0] < self.nActiveCollections:
                pc_item[1].show()
            else: 
                pc_item[1].hide()

    def hideInactives(self): 
        for pc_item in self.point_clouds.items():
            if pc_item[0] >= self.nActiveCollections:
                pc_item[1].hide()
            
    def getPoint( self, actor, iPt ):
        pc = self.point_cloud_map.get( actor, None )
        if pc: return pc.getPoint( iPt )
        else: return None, None

    def getTimeseries( self, actor, iPt ):
        pc = self.point_cloud_map.get( actor, None )
        if pc: return pc.getTimeseries( iPt )
        else: return None, None

    def printLogMessage(self, msg_str ):
        print " vtkPartitionedPointCloud: %s" % ( msg_str )
        sys.stdout.flush()      
            
    def getActors(self):
        return [ pc.actor for pc in self.point_clouds.values() ]
    
    def getSubsetSpecs(self):
        return self.current_spec[ PCProc.Subset ]
    
    def runProcess(self, procType, **args):
        process_spec = args.get( 'spec', None )
        if process_spec: 
            self.current_spec[ procType ] = process_spec
        else:            
            process_spec = self.current_spec.get( procType, None )
        pc_base_index = args.get( 'pc_base_index', 0 )
        allow_processing =  args.get( 'allow_processing', True )
        if self.nActiveCollections > pc_base_index:
            self.clearProcQueues()
            for pc_item in self.point_clouds.items():
                run_process = allow_processing and ( pc_item[0] < self.nActiveCollections ) and ( pc_item[0] >= pc_base_index )
                pc_item[1].runProcess( procType, spec=process_spec, process=run_process )
            self.startCheckingProcQueues()
    
    def generateSubset(self, **args ):
        self.runProcess( PCProc.Subset, **args )
 
    def generateZScaling(self, **args ):
        self.runProcess( PCProc.ZScaling, **args )
 
    def stepTime(self, **args ):
        self.runProcess( PCProc.Timestep, **args )
        update_points = args.get( 'update_points', True )
        if update_points: self.generateSubset()
      
    def clearProcQueues(self):
        for pc_item in self.point_clouds.items():
            if pc_item[0] < self.nActiveCollections:
                pc_item[1].clearQueues()
                
#    def setNewSubsetCallback( self, callback ):
#        'NewSubset'
                       
    def getPointCloud(self, pcIndex ):
        return self.point_clouds.get( pcIndex, None )

    def setScalarRange( self, scalar_range ):
        for pc in self.point_clouds.values():
            pc.setScalarRange( scalar_range )
            
    def applyColorRange( self, range_type ):
        color_range = [ float("inf"), -float("inf") ]
        for pc in self.point_clouds.values():
            crange = pc.getValueRange( None, range_type )
            color_range[0] = min( color_range[0], crange[0])
            color_range[1] = max( color_range[1], crange[1])
        self.setScalarRange( color_range )
        return color_range
            
    def getNLevels(self):
        for pc in self.point_clouds.values():
            nlev = pc.getNLevels()
            if nlev: return nlev
        return None
        
    def setPointSize( self, point_size ) :  
        for pc in self.point_clouds.values():
            pc.setPointSize( point_size )

    def getPointSize( self ) :  
        pclist = self.point_clouds.values()
        return pclist[0].getPointSize()
    
    def values(self):
        return self.point_clouds.values()
    
    def setTopo( self, topo, **args ):
        pts = None
        for pc_item in self.point_clouds.items():
            if pc_item[0] < self.nActiveCollections:
                pts = pc_item[1].setTopo( topo, **args )
        return pts

    def postDataQueueEvent( self ):
        self.startCheckingProcQueues()
#        pass
#        QtCore.QCoreApplication.postEvent( self, QtCore.QTimerEvent( self.dataQueueTimer ) ) 
    

def kill_all_zombies():
#                                              Cleanup abandoned processes
    import subprocess, signal    
    proc_specs = subprocess.check_output('ps').split('\n')
    for proc_spec in proc_specs:
        if 'UVIS_DV3D' in proc_spec or 'uvcdat' in proc_spec:
            pid = int( proc_spec.split()[0] )
            if pid <> os.getpid():
                os.kill( pid, signal.SIGKILL )
                print "Killing proc: ", proc_spec

    
        
        
            