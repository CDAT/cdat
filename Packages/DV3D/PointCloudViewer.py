'''
Created on Aug 29, 2013

@author: tpmaxwel
'''

import sys, cdms2
import os.path, traceback, threading, multiprocessing
import vtk, time
from DistributedPointCollections import vtkPartitionedPointCloud, vtkLocalPointCloud, ScalarRangeType
from ConfigurationFunctions import *
from ColorMapManager import *
from MapManager import MapManager
from MultiVarPointCollection import InterfaceType, PlotType
from DV3DPlot import *

VTK_NO_MODIFIER         = 0
VTK_SHIFT_MODIFIER      = 1
VTK_CONTROL_MODIFIER    = 2        
VTK_TITLE_SIZE = 14
VTK_INSTRUCTION_SIZE = 24

def getVarName( var ):
    if hasattr( var,'name_in_file'): return var.name_in_file 
    if hasattr( var,'name'): return var.name 
    if hasattr( var,'id'): return var.id 
    if hasattr( var,'outvar'): return var.outvar.name 
        
def dump_np_array1( a, label=None ):
    print "\n-------------------------------------------------------------------------------------------------"
    if label: print label
    npts = a.shape[0]
    nrows = 20
    iSkip = npts/nrows
    for ir in range(nrows):
        iPt = iSkip*ir
        print "Pt[%d]: %f  " % ( iPt, a[ iPt ])
    print "-------------------------------------------------------------------------------------------------\n"
    for ir in range(nrows):
        iPt =  npts/2 + ir
        print "Pt[%d]: %f " % ( iPt, a[ iPt ] )
    print "-------------------------------------------------------------------------------------------------\n"

def dump_np_array3( a, label=None ):
    print "\n-------------------------------------------------------------------------------------------------"
    if label: print label
    npts = a.shape[0]/3
    nrows = 20
    iSkip = npts/nrows
    for ir in range(nrows):
        iPt = iSkip*ir
        ioff = iPt*3
        print "Pt[%d]: %.2f %.2f, %.2f " % ( iPt, a[ ioff ], a[ ioff+1 ], a[ ioff+2 ] )
    print "-------------------------------------------------------------------------------------------------\n"
    for ir in range(nrows):
        iPt =  npts/2 + ir
        ioff = iPt*3
        print "Pt[%d]: %.2f %.2f, %.2f " % ( iPt, a[ ioff ], a[ ioff+1 ], a[ ioff+2 ] )
    print "-------------------------------------------------------------------------------------------------\n"

def dump_vtk_array3( a, label=None ):
    print "\n-------------------------------------------------------------------------------------------------"
    if label: print label
    npts = a.GetNumberOfTuples()
    nrows = 20
    iSkip = npts/nrows
    for ir in range(nrows):
        iPt = iSkip*ir
        pt = a.GetTuple(iPt)
        print "Pt[%d]: %.2f %.2f, %.2f " % ( iPt, pt[ 0 ], pt[ 1 ], pt[ 2 ] )
    print "-------------------------------------------------------------------------------------------------\n"
    for ir in range(nrows):
        iPt =  npts/2 + ir
        pt = a.GetTuple(iPt)
        print "Pt[%d]: %.2f %.2f, %.2f " % ( iPt, pt[ 0 ], pt[ 1 ], pt[ 2 ] )
    print "-------------------------------------------------------------------------------------------------\n"

def dump_vtk_array1( a, label=None ):
    print "\n-------------------------------------------------------------------------------------------------"
    if label: print label
    npts = a.GetSize()
    nrows = 20
    iSkip = npts/nrows
    for ir in range(nrows):
        iPt = iSkip*ir
        v = a.GetValue(iPt)
        print "Pt[%d]: %.2f  " % ( iPt, v )
    print "-------------------------------------------------------------------------------------------------\n"
    for ir in range(nrows):
        iPt =  npts/2 + ir
        v = a.GetValue(iPt)
        print "Pt[%d]: %.2f " % ( iPt, v )
    print "-------------------------------------------------------------------------------------------------\n"
    
def dump_vtk_points( pts, label=None ):
    print "\n-------------------------------------------------------------------------------------------------"
    npts = pts.GetNumberOfPoints()
    if label: print label
    nrows = 20
    iSkip = npts/nrows
    for ir in range(nrows):
        iPt = iSkip*ir
        pt = pts.GetPoint( iPt )
        print "Pt[%d]: %.2f %.2f, %.2f " % ( iPt, pt[ 0 ], pt[ 1 ], pt[ 2 ] )
    print "-------------------------------------------------------------------------------------------------\n"
    for ir in range(nrows):
        iPt =  npts/2 + ir
        pt = pts.GetPoint( iPt )
        print "Pt[%d]: %.2f %.2f, %.2f " % ( iPt, pt[ 0 ], pt[ 1 ], pt[ 2 ] )
    print "-------------------------------------------------------------------------------------------------\n"
    

class ConfigMode:
    Default = 0
    Color = 1
    Points = 2
     

class Counter(): 
    
    def __init__( self, maxvalue = 0, minvalue = 0 ):
        self.floor = minvalue
        self.ceiling = maxvalue
        self.index = self.ceiling 
        self.active = True
        
    def reset( self, ceiling = -1 ):
        if ceiling >= 0: self.ceiling = ceiling
        self.index = self.ceiling 
        self.active = True
        
    def isActive(self):
        return self.active
        
    def setFloor(self, minvalue):
        self.floor = minvalue

    def setCeiling(self, maxvalue):
        self.ceiling = maxvalue
        
    def value(self):
        return self.index
        
    def decrement(self ):
        self.index = self.index - 1
        if self.index <= self.floor:
            self.active = False
            return True
        return False
               
class CPCPlot( DV3DPlot ):  
    

    def __init__( self, **args ):
        DV3DPlot.__init__( self, **args  )
        self.ValueChanged = SIGNAL('ValueChanged')
        self.ConfigCmd = SIGNAL('ConfigCmd')
        self.sliceAxisIndex = -1
        self.partitioned_point_cloud = None
        self.point_cloud_overview = None
        self.scaling_spec = None
        self.infovisDialog = None
        self.config_mode = ConfigMode.Default
        self.topo = PlotType.Planar
        self.zSliceWidth = 0.005
        self.sliceWidthSensitivity = [ 0.005, 0.005, 0.005 ]
        self.slicePositionSensitivity = [ 0.025, 0.025, 0.025 ]
        self.nlevels = None
        self.render_mode = ProcessMode.HighRes
        self.colorRange = 0 
        self.cmdSkipIndex = 0
        self.cmdSkipFactor = 3
        self.skipIndex = 1
        self.deactivate_low_res_actor = False
#        self.resolutionCounter = Counter()
        self._current_subset_specs = {}
        self.sphere_source = None
        self.vertVar = 'default' 
        
        interactionButtons = self.getInteractionButtons()
        interactionButtons.addSliderButton( names=['ScaleColormap'], key='C', toggle=True, label='Colormap Scale', interactionHandler=self.processColorScaleCommand )
        interactionButtons.addSliderButton( names=['ScaleTransferFunction'], key='T', toggle=True, parents=['ToggleVolumePlot'], label='Transfer Function Range', interactionHandler=self.processThresholdRangeCommand )
        interactionButtons.addSliderButton( names=['ScaleOpacity'], key='o', toggle=True, label='Opacity Scale', range_bounds=[ 0.0, 1.0 ], initValue=[ 1.0, 1.0 ], interactionHandler=self.processOpacityScalingCommand )
#        interactionButtons.addSliderButton( names=['IsosurfaceValue'], key='L', toggle=True, parents=['ToggleSurfacePlot'], sliderLabels='Isosurface Value', label='Positioning Isosurface', interactionHandler=self.processIsosurfaceValueCommand )
        interactionButtons.addSliderButton( names=['PointSize'], key='P', toggle=True, label='Point Size', sliderLabels=['Low Resolution', 'High Resolution' ], interactionHandler=self.processPointSizeCommand, range_bounds=[ 1, 12 ], initValue=[ 5, 1 ] )
        interactionButtons.addSliderButton( names=['SliceThickness'], key='w', toggle=True, label='Slice Thickness', sliderLabels=['Low Resolution', 'High Resolution' ], interactionHandler=self.processSlicePropertiesCommand, range_bounds=[  0.001, 0.01], initValue=[ 0.0025, 0.005 ] )
        interactionButtons.addConfigButton( names=['ToggleSphericalProj'], key='s', toggle=True, interactionHandler=self.processProjectionCommand )
        plotButtons = self.fetchPlotButtons()
  
#        self.addConfigurableSliderFunction( 'colorScale', 'C', label='Colormap Scale', interactionHandler=self.processColorScaleCommand )
#        self.addConfigurableSliderFunction( 'thresholding', 'T', label='Thresholding Range', interactionHandler=self.processThresholdRangeCommand )
#        self.addConfigurableSliderFunction( 'sliceProp', 'w', label='Slice Thickness', sliderLabels=['Low Resolution', 'High Resolution' ], interactionHandler=self.processSlicePropertiesCommand, range_bounds=[ 0.001, 0.01 ], initValue=[ 0.0025, 0.005 ] )        
#        self.addConfigurableSliderFunction( 'opacityScale', 'o', label='Opacity Scale', range_bounds=[ 0.0, 1.0 ], initValue=[ 1.0, 1.0 ], interactionHandler=self.processOpacityScalingCommand )
        
     
#        self.addConfigurableSliderFunction( 'slicing', 'C', label='Colormap Scale', interactionHandler=self.processColorScaleCommand )
#        self.addConfigurableLevelingFunction( 'map_opacity', 'M', label='Base Map Opacity', rangeBounds=[ 0.0, 1.0 ],  setLevel=self.setMapOpacity, activeBound='min',  getLevel=self.getMapOpacity, isDataValue=False, layerDependent=True, group=ConfigGroup.BaseMap, bound = False )

                 
    def toggleProjection( self, args, config_function  ):
        if len( args ) > 1: 
            self.toggleTopo( state = args[1] ) 
        else: 
            self.toggleTopo() 
        
    def processTimerEvent(self, caller, event):
        DV3DPlot.processTimerEvent(self, caller, event)
        eid = caller.GetTimerEventId ()
        etype = caller.GetTimerEventType()
        if etype == vtkPartitionedPointCloud.TimerType:
            if self.partitioned_point_cloud.processTimerEvent(eid):
                if self.deactivate_low_res_actor:
                    self.low_res_actor.VisibilityOff()
                    self.deactivate_low_res_actor = False
#                    print " ** deactivate_low_res_actor ** "

#         if etype == 11:
#             self.printInteractionStyle('processTimerEvent')

    def processSurfacePlotCommand( self, args, config_function = None ):
        DV3DPlot.processSurfacePlotCommand( self, args, config_function  )
        volumeParam = config_function.value
        if args and args[0] == "Init":
            vrange = self.point_cloud_overview.getValueRange()
            config_function.initial_value = vrange
            dvar = self.defvar[0] if ( type(self.defvar) == list ) else self.defvar
            volumeParam.setValue( 'range', vrange )
            volumeParam.setValue( dvar, vrange )

    def processVolumePlotCommand( self, args, config_function = None ):
        volumeParam = config_function.value
        DV3DPlot.processVolumePlotCommand( self, args, config_function  )
        if args and args[0] == "Init":
            vrange = self.point_cloud_overview.getValueRange()
            config_function.initial_value = vrange
            dvar = self.defvar[0] if ( type(self.defvar) == list ) else self.defvar
            volumeParam.setValue( 'range', vrange )
            volumeParam.setValue( dvar, vrange )
            
        
    @property
    def current_subset_specs(self):
        return self._current_subset_specs

    @current_subset_specs.setter
    def current_subset_specs(self, value):
        print "Setting current_subset_specs: ", str( value )
        self._current_subset_specs = value
                
        
    def toggleRenderMode( self ):     
        new_render_mode = ( self.render_mode + 1 ) % 2 
        self.setRenderMode( new_render_mode )
        self.render()
        
    def getPointCloud( self, ires = -1 ):
        if ires == -1: ires = self.render_mode
        if ( ( ires ==  ProcessMode.HighRes ) and ( self.partitioned_point_cloud <> None ) ):
            return self.partitioned_point_cloud  
        else:
            return  self.point_cloud_overview 

    def getPointClouds(self):
        return [ self.point_cloud_overview, self.partitioned_point_cloud ]
       
    def setRenderMode( self, render_mode ): 
        modes = [ 'lowres', 'highres' ]
#        print " Set render mode: ", modes[render_mode]
        if (render_mode == ProcessMode.HighRes):
            if ( self.partitioned_point_cloud == None ): return 
            if not self.partitioned_point_cloud.hasActiveCollections(): return            
        self.render_mode = render_mode    
        if render_mode ==  ProcessMode.HighRes:
            self.deactivate_low_res_actor = True
        else: 
            if self.partitioned_point_cloud:
                self.partitioned_point_cloud.clear()
#            self.refreshPointSize()
            self.deactivate_low_res_actor = False
            self.low_res_actor.VisibilityOn()  
            
    def getSphere(self):
        if self.sphere_source == None:
            self.createSphere()
        return self.sphere_source
    
    def configSphere( self, center, color ):
        sphere = self.getSphere()
        sphere.SetCenter( center )        
        self.sphere_actor.GetProperty().SetColor(color)

    def createSphere(self, center=(0,0,0), radius=0.2 ):
        self.sphere_source = vtk.vtkSphereSource()
        self.sphere_source.SetCenter(center)
        self.sphere_source.SetRadius(radius)        
        # mapper
        mapper = vtk.vtkPolyDataMapper()
        if vtk.VTK_MAJOR_VERSION <= 5:  mapper.SetInput(self.sphere_source.GetOutput())
        else:                           mapper.SetInputConnection(self.sphere_source.GetOutputPort())
        self.sphere_actor = vtk.vtkActor()
        self.sphere_actor.SetMapper(mapper)
        self.renderer.AddActor( self.sphere_actor )
         
    def onRightButtonPress( self, caller, event ):
        shift = caller.GetShiftKey()
        if not shift: return
        x, y = caller.GetEventPosition()
        print "Executing pick."
        picker = caller.GetPicker()
        picker.Pick( x, y, 0, self.renderer )
        actor = picker.GetActor()
        if actor:
            iPt = picker.GetPointId()
            if iPt >= 0:
                if self.partitioned_point_cloud and self.partitioned_point_cloud.hasActiveCollections():                
                    pick_pos, dval = self.partitioned_point_cloud.getPoint( actor, iPt ) 
                    color = self.getColormapManager().getColor( dval )
                    self.configSphere( pick_pos, color )
                else:
                    pick_pos, dval = self.point_cloud_overview.getPoint( iPt ) 
#                 if self.topo == PlotType.Spherical:
#                     pick_pos = glev.vtk_points_data.GetPoint( iPt )
#                 else:
#                     pick_pos = picker.GetPickPosition()                                     
                if pick_pos:        text = " Point[%d] ( %.2f, %.2f ): %s " % ( iPt, pick_pos[0], pick_pos[1], dval )
                else:               text = "No Pick"
                self.updateTextDisplay( text )
                
                if self.configDialog.plotting():
                    tseries = self.partitioned_point_cloud.getTimeseries( actor, iPt ) 
                    self.configDialog.pointPicked( tseries, pick_pos )       
            
    def toggleTopo( self, **args ):
        state = args.get( 'state', None )
        self.topo = ( self.topo + 1 ) % 2 if (state == None) else state
        self.updateProjection()
        
    def updateProjection(self):
        self.recordCamera()
        pts =  [  self.partitioned_point_cloud.setTopo( self.topo ) if self.partitioned_point_cloud else False,
                  self.point_cloud_overview.setTopo( self.topo)    ] 
        if pts[self.render_mode]:
            self.resetCamera( pts[self.render_mode] )
            self.renderer.ResetCameraClippingRange()   
        if ( self.topo == PlotType.Spherical ): self.setFocalPoint( [0,0,0] )
#        self.enableSlicing() 
        self.mapManager.setMapVisibility( self.topo )
        self.render()
        

    def getMetadata(self):
        return self.point_cloud_overview.getMetadata()
            
    def onKeyEvent(self, eventArgs ):
        key = eventArgs[0]
        keysym =  eventArgs[1]            
        if keysym == "t":  self.stepTime()
        elif keysym == "A":  self.stepTime( False )
        elif keysym == "k":  self.toggleClipping()
        elif keysym == "m":  self.toggleRenderMode()
#        elif keysym == "v":  self.enableThresholding()
#        elif keysym == "i":  self.setPointIndexBounds( 5000, 7000 )
        else: return False
        return True
    
    def toggleVolumeVisibility( self, args, config_function ):
        if len( args ) > 1 and args[1]:
            if (len(args) > 3):
                button_bar = args[3]
                button_bar.clear( current=config_function.name )
            self.render()

    def toggleIsosurfaceVisibility( self, args, config_function ):
        
        return
    
        if len( args ) > 1 and args[1]:
            if (len(args) > 3):
                button_bar = args[3]
                button_bar.clear( current=config_function.name )
            self.render()
                         
    def processCategorySelectionCommand( self, args ):
        op = args[0]
        if op == 'Subsets':
            if (self.process_mode == ProcessMode.Slicing) or ( len(self._current_subset_specs) == 0 ): 
                self.enableSlicing()
            elif self.process_mode == ProcessMode.Thresholding:  
                self.enableThresholding()
        elif op == 'Color':
            self.enableColorConfig() 
        elif op == 'Points':
            self.enablePointConfig() 
                
    def enableColorConfig(self):
        self.config_mode = ConfigMode.Color

    def enablePointConfig(self):
        self.config_mode = ConfigMode.Points            

    def processAnimationCommand( self, args ):
        if args and args[0] == "ButtonClick":
            if args[1]   == "Run":
                pass
            elif args[1] == "Step":
                thresholding = (self.process_mode == ProcessMode.Thresholding)
                if self.partitioned_point_cloud: 
                    self.partitioned_point_cloud.stepTime( update_points=thresholding )
                    self.point_cloud_overview.stepTime( process= not self.partitioned_point_cloud.hasActiveCollections(), update_points=thresholding )
                else:
                    self.point_cloud_overview.stepTime( process=True, update_points=thresholding)
                    
                self.render() 
            elif args[1] == "Stop":
                pass

    def stepTime( self, forward = True ):
#         ntimes = len(self.time) if self.time else 1
#         self.iTimeStep = self.iTimeStep + 1 if forward else self.iTimeStep - 1
#         if self.iTimeStep < 0: self.iTimeStep = ntimes - 1
#         if self.iTimeStep >= ntimes: self.iTimeStep = 0
#         try:
#             tvals = self.time.asComponentTime()
#             tvalue = str( tvals[ self.iTimeStep ] )
#             self.updateTextDisplay( "Time: %s " % tvalue )
#         except Exception, err:
#             print>>sys.stderr, "Can't understand time metadata."
#         np_var_data_block = self.getDataBlock()
#         var_data = np_var_data_block[:] if ( self.nLevels == 1 ) else np_var_data_block[ :, : ]    
#         self.setVarData( var_data ) 
#         self.renderWindow.Render()
#         thresholding = (self.process_mode == ProcessMode.Thresholding)
#         if self.partitioned_point_cloud: 
#             self.partitioned_point_cloud.stepTime( update_points=thresholding )
#             self.point_cloud_overview.stepTime( process= not self.partitioned_point_cloud.hasActiveCollections(), update_points=thresholding )
#         else:
#             self.point_cloud_overview.stepTime( process=True, update_points=thresholding)

        thresholding = (self.process_mode == ProcessMode.Thresholding)
        self.setRenderMode( ProcessMode.LowRes )   
        self.point_cloud_overview.stepTime( process=True, update_points=thresholding)
        self.low_res_actor.VisibilityOn()                                    
        self.render() 

    def update_subset_specs(self, new_specs ):
        print " $$$$$$ update_subset_specs: %s " % str( new_specs )
        self.current_subset_specs.update( new_specs )

            
#     def processSlicePlaneCommand( self, args ):
# #        print " processSlicePlaneCommand: %s " % str( args )
#         if args and args[0] == "StartConfig":
#             scalarRange = self.getConfigFunction('colorScale').value 
#             if self.render_mode ==  ProcessMode.HighRes:
#                 title = args[2]
#                 if not (title in SLICE_WIDTH_HR_COMP):
#                     self.setRenderMode( ProcessMode.LowRes, True )
#             self.point_cloud_overview.setScalarRange( scalarRange.getValues() )               
#             if self.partitioned_point_cloud:
#                 self.update_subset_specs(  self.partitioned_point_cloud.getSubsetSpecs()  )            
#                 self.point_cloud_overview.generateSubset( spec=self.current_subset_specs ) 
#                 self.configDialog.newSubset( self.point_cloud_overview.getCellData() )       
#         elif args and args[0] == "EndConfig":
#             self.setRenderMode( ProcessMode.HighRes )                 
#             self.execCurrentSlice()
#         
#         elif args and args[0] == "Open":
#             self.enableSlicing()
#         elif args and args[0] == "UpdateTabPanel":
#             axis_index = args[1]
#             if axis_index == 2:
#                 slice_index = round( self.getSlicePosition() / self.zSliceWidth ) 
#                 self.setSlicePosition( slice_index * self.zSliceWidth )   
#                 self.execCurrentSlice()           
#         elif args and args[0] == "Close":
#             isOK = args[1]
#             self.setRenderMode( ProcessMode.HighRes )
# #            if isOK: self.getPointCloud().setScalarRange( self.scalarRange.getValues() )              
#             self.render()
#         elif args and args[0] == "SelectSlice":
#             self.sliceAxisIndex =  args[1]
#             self.enableSlicing()  
            
    def processVariableCommand( self, args = None ):
        print " -------------------.>> Process Variable Command: ", str( args ) 
        self.processThresholdRangeCommand( args ) 

    @property
    def defvar(self):
        return self.point_cloud_overview.point_collection.var.id 
    
    def processSlicingCommand( self, args, config_function = None ):  
#        print " Process Slicing Command: " , str( args )   
        sliceParam = config_function.value
        if args and args[0] == "StartConfig":            
            self.setRenderMode( ProcessMode.LowRes )   
            self.execCurrentSlice()   
            self.low_res_actor.VisibilityOn()                        
        elif args and args[0] == "Init":
            axis_bounds = self.point_cloud_overview.getAxisBounds()
            config_function.initial_value = axis_bounds
            config_function.setRangeBounds( axis_bounds )
            sliceParam.setValue( 'bounds', [ [ axis_bounds[0], axis_bounds[1] ], [ axis_bounds[2], axis_bounds[3] ], [ axis_bounds[4], axis_bounds[5] ] ] )
            sliceParam.setValue( 'spos', [ axis_bounds[0], axis_bounds[2], axis_bounds[4] ] )
            sliceParam.setValue( 0, axis_bounds[0] )
        elif args and args[0] == "EndConfig":
            positions = sliceParam.getValue( 'spos' )
            positions[self.sliceAxisIndex] = sliceParam.getValue()
#            print "Update slice value[%d]: %f " % ( self.sliceAxisIndex, sliceParam.getValue() )
            sliceParam.setValue( 'spos', positions )
            self.setRenderMode( ProcessMode.HighRes )            
            self.execCurrentSlice( )       
        elif args and args[0] == "InitConfig":
            if (len(args) > 1) and args[1]:
                self.clearSubsetting()
                if (len(args) > 3):
                    button_bar = args[3]
                    button_bar.clear( current=config_function.name )
                self.sliceAxisIndex = config_function.position[0]
                self.updateTextDisplay( config_function.label )
                self.process_mode = ProcessMode.Slicing
                self.setRenderMode( ProcessMode.HighRes )  
                positions = sliceParam.getValue( 'spos' )
                spos = positions[ self.sliceAxisIndex ]
                axis_bounds = sliceParam.getValue( 'bounds' )
                config_function.setRangeBounds( axis_bounds[ self.sliceAxisIndex ] )
                sliceParam.setValue( 0, spos )
                self.execCurrentSlice( spos=spos )
                if self.partitioned_point_cloud:       
                    self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
        elif args and args[0] == "UpdateConfig":
            self.sliceAxisIndex = args[1]
            value = args[2].GetValue()
#            print "Set slice value: ", float( value )
            sliceParam.setValue( 0, value )
            self.execCurrentSlice(spos=value)
    
    def processThresholdRangeCommand( self, args, config_function = None ):
#        print " ---->>  processThresholdRangeCommand: %s[%d] " % ( args[0], self.cmdSkipIndex )
        volumeThresholdRange = config_function.value
        if args and args[0] == "StartConfig":
            if self.render_mode ==  ProcessMode.HighRes:
                self.setRenderMode( ProcessMode.LowRes )             
            if self.partitioned_point_cloud:       
                self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
            if self.process_mode <> ProcessMode.Thresholding:
                self.enableThresholding(volumeThresholdRange)        
        elif args and args[0] == "Init":
            init_range = self.point_cloud_overview.getValueRange()
            config_function.setRangeBounds(  init_range   )
            config_function.initial_value = init_range  
            volumeThresholdRange.setValues( init_range )
            dvar = self.defvar[0] if ( type(self.defvar) == list ) else self.defvar
            volumeThresholdRange.setValue( dvar, init_range )
        elif args and args[0] == "EndConfig":
            self.setRenderMode( ProcessMode.HighRes )                
            self.updateThresholding()        
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
            dvar = self.defvar[0] if ( type(self.defvar) == list ) else self.defvar
            volumeThresholdRange.setValues( volumeThresholdRange.getValue( dvar ) )
            self.setRenderMode( ProcessMode.HighRes )             
            self.enableThresholding( volumeThresholdRange )
            if self.partitioned_point_cloud:       
                self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
        elif args and args[0] == "Open":
            self.enableThresholding(volumeThresholdRange)
        elif args and args[0] == "Close":
            isOK = args[1] if ( len( args ) > 1 ) else True
            if self.render_mode == ProcessMode.LowRes:
                self.setRenderMode( ProcessMode.HighRes )
                dvar = self.defvar[0] if ( type(self.defvar) == list ) else self.defvar
                if isOK: self.updateThresholding( dvar, volumeThresholdRange.getValue( dvar) )                
                self.render()
        elif args and args[0] == "UpdateConfig":
            if ( self.cmdSkipIndex % self.cmdSkipFactor ) == 0:
                value = args[2].GetValue()
                dvar = self.defvar[0] if ( type(self.defvar) == list ) else self.defvar
                vt_range = list( volumeThresholdRange.getValue( dvar ) )
                vt_range[ args[1] ] = value
                volumeThresholdRange.setValue( dvar, vt_range )
                volumeThresholdRange.setValues( vt_range )
                self.updateThresholding( dvar, vt_range, False )
            self.cmdSkipIndex = self.cmdSkipIndex + 1
                    
    def enableThresholding( self, volumeThresholdRange=None, **args ):
        self.updateTextDisplay( "Mode: Thresholding", True )
        self.cmdSkipIndex = 0
        self.clearSubsetting()
        self.process_mode = ProcessMode.Thresholding 
#         if self.render_mode ==  ProcessMode.LowRes:
#             self.setRenderMode( ProcessMode.HighRes )
#         if self.scalarRange <> None:
#             self.point_cloud_overview.setScalarRange( self.scalarRange.getValues() )
        if volumeThresholdRange <> None: 
            dvar = self.defvar[0] if ( type(self.defvar) == list ) else self.defvar
            self.updateThresholding( dvar, volumeThresholdRange.getValue(dvar), False )
#         bbar = self.fetchPlotButtons()
#         bbar.updateInteractionState( 'ToggleVolumePlot', 1 )
                   
    def processColorScaleCommand( self, args, config_function ):
        scalarRange = config_function.value
        if args and args[0] == "Init":
            init_range = self.point_cloud_overview.getValueRange()
            config_function.initial_value = init_range
            config_function.setRangeBounds(  init_range )
            self.point_cloud_overview.setScalarRange( init_range ) 
            self.setColorbarRange( init_range ) 
            scalarRange.setValues( init_range )            
        elif  args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
        elif args and args[0] == "StartConfig":
            if self.render_mode ==  ProcessMode.HighRes:
                self.setRenderMode( ProcessMode.LowRes )
            self.point_cloud_overview.setScalarRange( scalarRange.getValues() )               
            if self.partitioned_point_cloud: 
                self.update_subset_specs(  self.partitioned_point_cloud.getSubsetSpecs()  )          
                self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
        elif args and args[0] == "EndConfig":
            if self.render_mode ==  ProcessMode.LowRes:
                print " Color Scale End Config "      
                self.setRenderMode( ProcessMode.HighRes ) 
                pc =  self.getPointCloud()             
                pc.setScalarRange( scalarRange.getValues() )  
                pc.refresh(True) 
        elif args and args[0] == "UpdateConfig": 
            value = args[2].GetValue()
            scalarRange.setValue( args[1], value )
            srange = scalarRange.getValues()        
            self.point_cloud_overview.setScalarRange( srange ) 
            self.setColorbarRange( srange ) 
        self.render()

                     
#     def shiftThresholding( self, position_inc, width_inc ):
#         volumeThresholdRange = self.getConfigFunction('thresholding').value
#         self.updateThresholding( self.defvar, volumeThresholdRange.getValue(self.defvar) )

    def getSliceWidth(self, res, slice_index = -1  ):
        interactionButtons = self.getInteractionButtons()
        cf = interactionButtons.getConfigFunction('SliceThickness')
        sliceWidths = cf.value
        if slice_index == -1: slice_index = self.sliceAxisIndex
        if slice_index == 2: return self.zSliceWidth
        return sliceWidths.getValue( res )
            
    def getSlicePosition( self, **args ): 
        normalized = args.get( 'normalized', False )
        spos = args.get( 'spos', None )
        interactionState = [ 'XSlider', 'YSlider', 'ZSlider' ][self.sliceAxisIndex]
        bbar = self.fetchPlotButtons()
        config_function = bbar.getConfigFunction( interactionState )
        if spos == None:
            spos = config_function.value.getValue()
        if normalized: 
            bounds = config_function.getRangeBounds()
            axis_bounds = self.point_cloud_overview.getAxisBounds()
            sindex = 2*self.sliceAxisIndex 
#            spos =  ( spos - axis_bounds[sindex]) / ( axis_bounds[sindex+1] - axis_bounds[sindex])   
            spos =  ( spos - bounds[0]) / ( bounds[1] - bounds[0] )   
#            print "            >>--------------->>> Norm Slice Position: %s in %s " % ( str(spos), str(bounds) )
        return spos

#     def setSlicePosition(self, slice_pos ):  
#         widget_item = self.currentSliders.get( 0, None )
#         if widget_item == None: 
#             swidget = self.createSliderWidget(0) 
#         else:
#             ( process_mode, interaction_state, swidget ) = widget_item 
#         srep = swidget.GetRepresentation( )  
#         srep.SetValue(slice_pos)
                
#     def getCurrentSlicePosition(self):
#         bounds = self.point_cloud_overview.getBounds()
#         sindex = 2*self.sliceAxisIndex 
#         return bounds[sindex] + self.getSlicePosition() * ( bounds[sindex+1] - bounds[sindex] )
    
    def execCurrentSlice( self, **args ):
        args['normalized'] = True
        slice_bounds = self.getSliceBounds(**args)
        self.invalidate()
        self.clearSliceSpecs()
        ( rmin, rmax ) = slice_bounds[ self.render_mode ]
        subset_spec = ( self.sliceAxes[self.sliceAxisIndex], rmin, rmax, True )
        self.current_subset_specs[ self.sliceAxes[self.sliceAxisIndex] ] = subset_spec
        istyle = self.renderWindowInteractor.GetInteractorStyle()
        if self.render_mode ==  ProcessMode.HighRes:
#            print " HR ExecCurrentSlice: subset_spec = %s, args = %s, sbounds = %s " % ( str(subset_spec), str(args), str(slice_bounds) )
            self.partitioned_point_cloud.generateSubset( spec=self.current_subset_specs, allow_processing=True )
        else:
#            print " LR ExecCurrentSlice: subset_spec = %s, args = %s, sbounds = %s " % ( str(subset_spec), str(args), str(slice_bounds) )
            self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
#            if self.partitioned_point_cloud:
#                self.partitioned_point_cloud.generateSubset( spec=self.current_subset_specs, allow_processing=False )
#        self.configDialog.newSubset( self.point_cloud_overview.getCellData() )
        self.render( mode=self.render_mode )
        
    def getSliceBounds( self, **args ):
        slice_bounds = []
        spos = self.getSlicePosition( **args )
        for iRes in [ ProcessMode.LowRes, ProcessMode.HighRes ]:
            slice_radius = self.getSliceWidth( iRes ) # self.sliceWidth[self.sliceAxisIndex]/(iRes)     
            pmin = max( spos - slice_radius, 0.0 )
            pmin = min( pmin, 1.0 - slice_radius )
            pmax = min( spos + slice_radius, 1.0 )
            pmax = max( pmax, slice_radius )
            slice_bounds.append( (pmin,pmax) )
#        print " && ExecCurrentSlice, slice properties: %s " % ( str( self.sliceProperties ) ); sys.stdout.flush()
        return slice_bounds
    
#     def pushSlice( self, slice_pos ):
#         self.updateTextDisplay( " Slice Position: %s " % str( slice_pos ) )
#         self.setSlicePosition( slice_pos )
#         self.execCurrentSlice()

    def shiftResolution( self, ncollections_inc, ptsize_inc ):
        if (ncollections_inc <> 0) and ( self.partitioned_point_cloud <> None ):
            self.partitioned_point_cloud.updateNumActiveCollections( ncollections_inc )
        if ptsize_inc <> 0:
            self.updatePointSize( ptsize_inc )
            
    def clearSubsetting(self):
        self.current_subset_specs = {}
        
    def updateThresholding( self, target=None, trange=None, normalized=True ):
        if target <> None:
            subset_spec = ( target, trange[0], trange[1], normalized )
            self.current_subset_specs[target] = subset_spec
#            print " $$$$$$ Update Thresholding: Generated spec = %s, render mode = %d " % ( str( subset_spec ), self.render_mode )
#        else: print " Update Thresholding: render mode = %d " % ( self.render_mode )
        self.invalidate()
        pc = self.getPointCloud()
        pc.generateSubset( spec=self.current_subset_specs )
#         if (self.render_mode == ProcessMode.LowRes) and : 
#             self.configDialog.newSubset( self.point_cloud_overview.getCellData() )
        self.render( mode=self.render_mode )
        sys.stdout.flush()

#     def processConfigCmd( self, args ):
#         print " >>>>>>>>>> processConfigCmd: %s " % str(args); sys.stdout.flush()
#         if args[0] =='Color Scale':
#             self.processColorScaleCommand( args[1:] )
#         elif args[0] =='Animation':
#             self.processAnimationCommand( args[1:] )
#         elif args[0] =='Slice Planes':
#             self.processSlicePlaneCommand( args[1:] )
#         elif args[0] =='Threshold Range':
#             self.processThresholdRangeCommand( args[1:] )
#         elif args[0] =='CategorySelected':
#             self.processCategorySelectionCommand( args[1:] )
#         elif args[0] =='InitParm':
#             self.processsInitParameter( args[1], args[2] )
#         elif args[0] =='Point Size':
#             self.processPointSizeCommand( args[1:] )
#         elif args[0] =='Opacity Scale':
#             self.processOpacityScalingCommand( args[1:] )
#         elif args[0] =='Opacity Graph':
#             self.processOpacityGraphCommand( args[1:] )
#         elif args[0] =='Vertical Scaling':
#             self.processVerticalScalingCommand( args[1:] )
#         elif args[0] =='ROI':
#             self.processROICommand( args[1:] )
            
    def processROICommand( self, args ):
        print " process ROI Command: ", str( args )
        if args[0] == 'Submit':
            roi = args[1]
            self.partitioned_point_cloud.setROI( roi )  
            self.point_cloud_overview.setROI( roi )
            self.render()   

    def processPointSizeCommand( self, arg, config_function ):
        pointSize = config_function.value
        if arg and arg[0] == "Init":
            for resolution in range(2):
                pc = self.getPointCloud(resolution)
                pc.setPointSize( config_function.initial_value[resolution] )
        elif arg and arg[0] == "InitConfig":
                self.updateTextDisplay( config_function.label )
        elif arg[0] == 'StartConfig':
            render_mode = arg[1]
            self.setRenderMode( render_mode )
            if render_mode == ProcessMode.HighRes: 
                if self.partitioned_point_cloud:
                    self.partitioned_point_cloud.refresh(True)
            else:
#                self.point_cloud_overview.setScalarRange( scalarRange.getValues() ) 
                if self.partitioned_point_cloud:
                    self.update_subset_specs(  self.partitioned_point_cloud.getSubsetSpecs()  )          
                    self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
            self.render()
        elif arg[0] == 'EndConfig':
            self.setRenderMode( ProcessMode.HighRes )
            pc =  self.getPointCloud()             
#            pc.setScalarRange( scalarRange.getValues() )  
            pc.refresh(True) 
            self.render() 
        elif arg and arg[0] == "UpdateConfig": 
            value = arg[2].GetValue()
            resolution = arg[1]
            current_point_size = pointSize.getValue( resolution )  
            new_point_size = int( round( value ) )
            if (current_point_size <> new_point_size ):
                print " UpdateConfig, resolution = %s, new_point_size = %s " % ( str( resolution ), str( new_point_size ) )
                pointSize.setValue(resolution, new_point_size )      
                pc = self.getPointCloud(resolution)
                pc.setPointSize( new_point_size )
                self.setRenderMode( resolution )
                if resolution == ProcessMode.HighRes: 
                    if self.partitioned_point_cloud: self.partitioned_point_cloud.refresh(True)
                self.render( mode=resolution )

    def processSlicePropertiesCommand( self, arg, config_function ):
        sliceProp = config_function.value
        if arg and arg[0] == "InitConfig":
                self.updateTextDisplay( config_function.label )
        elif arg[0] == 'Init':
            for resolution in range(2):
                sliceProp.setValue( resolution, config_function.initial_value[ resolution ] ) 
        elif arg[0] == 'StartConfig':
            render_mode = arg[1]
            self.setRenderMode( render_mode )
            if render_mode == ProcessMode.HighRes: 
                if self.partitioned_point_cloud:
                    self.partitioned_point_cloud.refresh(True)
            else:
#                self.point_cloud_overview.setScalarRange( scalarRange.getValues() ) 
                if self.partitioned_point_cloud:
                    self.update_subset_specs(  self.partitioned_point_cloud.getSubsetSpecs()  )          
                    self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
            self.render()
        elif arg[0] == 'EndConfig':
            self.setRenderMode( ProcessMode.HighRes )
            pc =  self.getPointCloud()             
#            pc.setScalarRange( scalarRange.getValues() )  
            pc.refresh(True) 
            self.render() 
        elif arg and arg[0] == "UpdateConfig": 
            resolution = arg[1]
            new_slice_width = arg[2].GetValue()
            sliceWidth = sliceProp.getValue( resolution ) 
            if sliceWidth <> new_slice_width:
                sliceProp.setValue( resolution, new_slice_width )
                self.setRenderMode( resolution )
                self.execCurrentSlice()
                self.render( mode=resolution )
                                        
    def setColorbarRange( self, cbar_range, cmap_index=0 ):
        colormapManager = self.getColormapManager( index=cmap_index )
        colormapManager.setDisplayRange( cbar_range )   

    def processsInitParameter( self, parameter_key, config_param ):
        paramKeys = parameter_key.split(':') 
        if paramKeys[0] == 'Color':
            if paramKeys[1] == 'Color Scale':
                pass
#                self.scalarRange = config_param  
#                self.scalarRange.setScalingBounds( self.point_cloud_overview.getValueRange()  )  
#                self.scalarRange.ValueChanged.connect( self.processColorScaleCommand ) 
#                norm_range = self.scalarRange.getValues() 
#                self.point_cloud_overview.setScalarRange( norm_range )  
#                self.setColorbarRange( norm_range )              
            elif paramKeys[1] == 'Color Map':
                self.colorMapCfg = config_param 
#                self.colorMapCfg.ValueChanged.connect( self.processColorMapCommand ) 
                self.processColorMapCommand()
            elif paramKeys[1] == 'Opacity Scale':
                self.oscale = config_param   
#                self.oscale.ValueChanged.connect(  self.processOpacityScalingCommand ) 
        elif paramKeys[0] == 'Subsets':
            if paramKeys[1] == 'Slice Planes':
                pass
#                self.sliceProperties = config_param
#                self.sliceProperties.ValueChanged.connect(  self.processSlicePropertiesCommand )  
#                self.enableSlicing()
            elif paramKeys[1] == 'Threshold Range':
                pass
#                self.volumeThresholdRange[self.defvar] = config_param                 
#                config_param.setScalingBounds( self.point_cloud_overview.getValueRange()  ) 
#                config_param.ValueChanged.connect(  self.processThresholdRangeCommand )      
        elif paramKeys[0] == 'Analysis':
            if paramKeys[1] == 'Threshold Range':
#                try:
#                    vname = paramKeys[2]
#                    config_param.setScalingBounds( self.point_cloud_overview.getValueRange( vname )  ) 
#                    self.volumeThresholdRange[vname] = config_param                 
#                    config_param.ValueChanged.connect(  self.processThresholdRangeCommand )  
#                except AttributeError:
                    pass    
#                self.enableThresholding()
        elif paramKeys[0] == 'Points':
            if paramKeys[1] == 'Point Size':
#                self.pointSize = config_param 
                cats = config_param.getValue('cats') 
#                 self.pointSize.ValueChanged.connect(  self.processPointSizeCommand ) 
                for ires in [ ProcessMode.LowRes, ProcessMode.HighRes ]:
                    pt_size_data = cats[ires]
                    init_point_size = pt_size_data[4]
                    config_param.setValue(ires,init_point_size)  
                    pc = self.getPointCloud(ires)  
                    pc.setPointSize( init_point_size )                                 
            elif paramKeys[1] == 'Max Resolution':
                self.maxRes = config_param   
#                self.maxRes.ValueChanged.connect(  self.processMaxResolutionCommand ) 
        elif paramKeys[0] == 'Geometry':
            if paramKeys[1] == 'Projection':
                self.projection = config_param   
#                self.projection.ValueChanged.connect(  self.processProjectionCommand ) 
            elif paramKeys[1] == 'Vertical Scaling':
                self.vscale = config_param   
#                self.vscale.ValueChanged.connect(  self.processVerticalScalingCommand ) 
            elif paramKeys[1] == 'Vertical Variable':
                pass
#                self.vertVar = config_param   
#                self.vertVar.ValueChanged.connect(  self.processVerticalVariableCommand )
        elif paramKeys[0] == 'Variables':
            self.variables[ paramKeys[1] ] = config_param
#            config_param.ValueChanged.connect(  self.processVariableCommand )
                
    def processMaxResolutionCommand(self, args=None ):
        max_res_spec =  self.maxRes.getValue() 
        if self.partitioned_point_cloud:
            self.partitioned_point_cloud.setResolution( max_res_spec )
        if not self.partitioned_point_cloud.hasActiveCollections():
            self.render_mode = ProcessMode.LowRes
        self.render()
        
    def processIsosurfaceValueCommand(self, args, config_function ):
        pass

    def processOpacityScalingCommand(self, args, config_function ):
        oscale = config_function.value
        oval = list( oscale.getValues() )
        if args[0] == "Init":
            ival = config_function.initial_value
            colormapManager = self.getColormapManager()
            colormapManager.setAlphaRange( ival )
#            print "Set alpha init: ", str( ival )
        if args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
        elif args[0] == "UpdateConfig": 
            if ( self.cmdSkipIndex % self.cmdSkipFactor ) == 0:
                oval[ args[1] ] = args[2].GetValue()
                oscale.setValues( oval )       
                colormapManager = self.getColormapManager()
                alpha_range = colormapManager.getAlphaRange()
                if ( abs( oval[0] - alpha_range[0] ) > 0.1 ) or ( abs( oval[1] - alpha_range[0] ) > 0.1 ):
                    colormapManager.setAlphaRange( oval )
                    print "Set alpha range: ", str( oval )
                    self.render()
            self.cmdSkipIndex = self.cmdSkipIndex + 1
        elif args[0] == "StartConfig":            
            self.setRenderMode( ProcessMode.LowRes ) 
            self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
            self.cmdSkipIndex = 0  
        elif args[0] == "EndConfig":            
            self.setRenderMode( ProcessMode.HighRes ) 
            self.partitioned_point_cloud.generateSubset( spec=self.current_subset_specs, allow_processing=True )
            self.render()  
            
    def processOpacityGraphCommand(self, args=None ):
        colormapManager = self.getColormapManager()
        colormapManager.setAlphaGraph( args[0] )
        self.render()

#    def setZScale( self, zscale_data, **args ):
#        pass
#        self.vscale.setValue( 'value', zscale_data[1], True )
#        self.processVerticalScalingCommand( [ "UpdateConfig" ] )

    def startConfiguration( self, x, y, config_types ):
        DV3DPlot.startConfiguration( self, x, y, config_types ) 
#         if (self.InteractionState == 'zScale') and not self.configuring:
#             self.processVerticalScalingCommand( [ "StartConfig" ] )

    def endConfiguration( self ):
        DV3DPlot.endConfiguration( self ) 
#         if (self.InteractionState == 'zScale'):
#             self.processVerticalScalingCommand( [ "EndConfig" ] )
    
    def updateVerticalScaling(self): 
        self.point_cloud_overview.generateZScaling( spec=self.scaling_spec )
        if self.partitioned_point_cloud:
            self.partitioned_point_cloud.generateZScaling( spec=self.scaling_spec )
                                       
    def processVerticalScalingCommand( self, args, config_function ):
        vscale = config_function.value
        if args and args[0] == "StartConfig":
            if self.render_mode ==  ProcessMode.HighRes:
                self.setRenderMode( ProcessMode.LowRes ) 
                self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
                self.render( mode=self.render_mode )   
        elif args and args[0] == "EndConfig":
            self.scaling_spec = ( self.vertVar, vscale.getValue() )
            if self.partitioned_point_cloud:
                self.partitioned_point_cloud.generateZScaling( spec=self.scaling_spec )
            self.setRenderMode( ProcessMode.HighRes )
            self.render() 
        elif args and args[0] == "Init":
            ( xcenter, ycenter, xwidth, ywidth ) = self.point_cloud_overview.getCenter()
#            val = config_function.initial_value[0]
            vscale_val = ( xwidth + ywidth )/500.0
            config_function.setRangeBounds( [ vscale_val/5.0, vscale_val*5.0 ] )
            vscale.setValues( [ vscale_val ] ) 
            self.scaling_spec = ( self.vertVar, vscale_val )
            self.skipIndex = 5
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )                      
            vscale.setValue( 'count', 1 ) 
        elif args and args[0] == "UpdateConfig":  
            count = vscale.incrementValue( 'count' )
            if count % self.skipIndex == 0:
                val = args[2].GetValue()         
                vscale.setValue( 0, val ) 
                self.scaling_spec = ( self.vertVar, val )
                self.point_cloud_overview.generateZScaling( spec=self.scaling_spec )
                self.render()

#     def processVerticalScalingCommand1( self, args, config_function ):
#         vscale = config_function.value
#         if args and args[0] == "StartConfig":
#             if self.render_mode ==  ProcessMode.HighRes:
#                 self.setRenderMode( ProcessMode.LowRes ) 
#                 self.point_cloud_overview.generateSubset( spec=self.current_subset_specs )
#                 self.render( mode=self.render_mode )   
#         elif args and args[0] == "Init":
#             ( xcenter, ycenter, xwidth, ywidth ) = self.point_cloud_overview.getCenter()
# #            val = config_function.initial_value[0]
#             val = ( xwidth + ywidth )/250.0
#             vscale.setValues( [ val ] ) 
#             print "^^^^^^^^^^^^^^^^^^^^^^^^^ SET ZSCALE: %s " % str( ( val, xwidth, ywidth ) )
#             scaling_spec = ( self.vertVar, val )
#             self.point_cloud_overview.generateZScaling( spec=scaling_spec )
#             vscale.setValue( 'count', 1 )
#         elif args and args[0] == "EndConfig":
#             scaling_spec = ( self.vertVar, vscale.getValue() )
#             if self.partitioned_point_cloud:
#                 self.partitioned_point_cloud.generateZScaling( spec=scaling_spec )
#             self.setRenderMode( ProcessMode.HighRes )
#             self.render() 
#         elif args and args[0] == "InitConfig":
#             axis_bounds = self.point_cloud_overview.getAxisBounds()
#             config_function.setRangeBounds( axis_bounds[4:] )
#             print "processVerticalScalingCommand.setRangeBounds: ", str( axis_bounds[4:]  )
#             self.updateTextDisplay( config_function.label )  
#             self.skipIndex = 5                   
#         elif args and args[0] == "UpdateConfig":  
#             count = vscale.incrementValue( 'count' )
#             if count % self.skipIndex == 0:
#                 val = args[2].GetValue()         
#                 vscale.setValue( 0, val ) 
#                 scaling_spec = ( self.vertVar, val )
#                 self.point_cloud_overview.generateZScaling( spec=scaling_spec )
#                                
#                 axis_bounds = self.point_cloud_overview.getAxisBounds()
#                 print "processVerticalScalingCommand.UpdateConfig: ", str( axis_bounds[4:]  ) 
#                 self.render()

                   
    def processVerticalVariableCommand(self, args=None ):
        scaling_spec = ( self.vertVar, self.vscale.getValue() )
        if self.partitioned_point_cloud:
            self.partitioned_point_cloud.generateZScaling( spec=scaling_spec )
        self.point_cloud_overview.generateZScaling( spec=scaling_spec )
        self.setRenderMode( ProcessMode.HighRes )
        self.render() 

    def processProjectionCommand( self, args, config_function  ): 
        if args and args[0] == "InitConfig": 
            self.toggleProjection( args, config_function )
            self.setRenderMode( ProcessMode.HighRes )
            self.render() 
                
    def processSelectedProjection( self ):
        seleted_projection = self.projection.getValue('selected')
        projections = self.projection.getValue('choices',[])
        try:
            self.topo = projections.index( seleted_projection )
            self.updateProjection()
        except ValueError:
            print>>sys.stderr, "Can't find projection: %s " % str( seleted_projection )

    def processColorMapCommand( self, args=None ):
        colorCfg = [ self.colorMapCfg.getValue('Colormap'), self.colorMapCfg.getValue('Invert'), self.colorMapCfg.getValue('Stereo'), self.colorMapCfg.getValue('Colorbar') ]
        self.setColormap( colorCfg ) 
        
    def clearSliceSpecs(self): 
        for s_axis in self.sliceAxes: 
            if s_axis in self.current_subset_specs: 
                del self.current_subset_specs[ s_axis ]

    def enableRender(self, **args ):
        onMode = args.get( 'mode', ProcessMode.AnyRes )
        return (onMode == ProcessMode.AnyRes) or ( onMode == self.render_mode )

#    def updateSlicing1( self, sliceIndex, slice_bounds ):
#        self.invalidate()
#        for iRes, pc in enumerate( self.getPointClouds() ):
#            ( rmin, rmax ) = slice_bounds[iRes]
#            pc.generateSubset( ( self.sliceAxes[sliceIndex], rmin, rmax ) )
#        self.render( mode=ProcessMode.LowRes )
                  
#                            
#    def plot( self, data_file, grid_file, varname, **args ): 
#        color_index = args.get( 'color_index', -1 )
#        self.inverted_levels = False
#        self.topo = args.get( 'topo', PlotType.Spherical )
#        npts_cutoff = args.get( 'max_npts', -1 )
#        ncells_cutoff = args.get( 'max_ncells', -1 )
#        self.iVizLevel = args.get( 'level', 0 )
#        self.z_spacing = args.get( 'z_spacing', 1.5 )
#        self.roi = args.get( 'roi', None )
#        
#        gf = cdms2.open( grid_file ) if grid_file else None
#        df = cdms2.open( data_file )       
#        lon, lat = self.getLatLon( df, varname, gf )
#        data_format = args.get( 'data_format', self.getDataFormat( df ) )
#                              
#        self.var = df[ varname ]
#        self.time = self.var.getTime()
#        self.lev = self.var.getLevel()
#        self.grid = self.var.getGrid()
#        missing_value = self.var.attributes.get( 'missing_value', None )
#        if self.lev == None:
#            domain = self.var.getDomain()
#            for axis in domain:
#                if PlotType.isLevelAxis( axis[0].id.lower() ):
#                    self.lev = axis[0]
#                    break
#                
#        np_var_data_block = self.getDataBlock()
#        
#        point_layout = self.getPointsLayout()
#        if point_layout == PlotType.Grid:
#            self.sliceThickness =[ (self.lon_data[1]-self.lon_data[0])/2.0, (self.lat_data[1]-self.lat_data[0])/2.0 ]
#
#        self.clippingPlanes = vtk.vtkPlanes()
#        if missing_value: var_data = numpy.ma.masked_equal( np_var_data_block, missing_value, False )
#        else: var_data = np_var_data_block
#        self.createThresholdedPolydata( lon=self.lon_data, lat=self.lat_data )
#        lut = self.get_LUT( invert = True, number_of_colors = 1024 )
#        self.setVarData( var_data, lut )          
#        self.createVertices()
#        if self.cropRegion == None: 
#            self.cropRegion = self.getBounds()                                                                                   
#        self.createRenderer( **args )
#        self.moveSlicePlane() 
#        if (self.topo == PlotType.Spherical): self.CreateMap()               
#
#    def plotPoints( self, proc_exec, data_file, grid_file, varname, **args ):
#        gf = cdms2.open( grid_file ) if grid_file else None
#        df = cdms2.open( data_file )       
#        lon, lat = self.getLatLon( df, varname, gf )  
#        self.var = df[ varname ]
#        self.time = self.var.getTime()
#        self.lev = self.var.getLevel()
#        self.grid = self.var.getGrid()
#        self.proc_exec = proc_exec
#        
#        if self.lev == None:
#            domain = self.var.getDomain()
#            for axis in domain:
#                if PlotType.isLevelAxis( axis[0].id.lower() ):
#                    self.lev = axis[0]
#                    break
#        
#        lut = self.get_LUT( invert = True, number_of_colors = 1024 )
#        for iCore in range(proc_exec.ncores):        
#            self.createPolydata( icore=iCore, lut=lut )
#            self.setVarData( icore=iCore )          
#            self.createVertices( icore=iCore )                                                                                   
#        self.createRenderer( **args )
#
#    def plotProduct( self, data_file, grid_file, varname, **args ): 
#        gf = cdms2.open( grid_file ) if grid_file else None
#        df = cdms2.open( data_file )       
#        lon, lat = self.getLatLon( df, varname, gf )  
#        self.var = df[ varname ]
#        self.time = self.var.getTime()
#        self.lev = self.var.getLevel()
#        self.grid = self.var.getGrid()
#        missing_value = self.var.attributes.get( 'missing_value', None )
#        if self.lev == None:
#            domain = self.var.getDomain()
#            for axis in domain:
#                if PlotType.isLevelAxis( axis[0].id.lower() ):
#                    self.lev = axis[0]
#                    break                                                                             
#        self.createRenderer( **args )          
#
#    def setSliceClipBounds( self, sliceProperties = 0.0 ):
#        if self.lev == None: return
#        bounds = self.getBounds()
#        mapperBounds = None
#        if self.sliceOrientation == 'x':
#            lev_bounds = [ 0, len( self.lev  ) * self.z_spacing ]
#            mapperBounds = [ sliceProperties-self.sliceThickness[0],  sliceProperties+self.sliceThickness[0], bounds[2], bounds[3], lev_bounds[0], lev_bounds[1]  ]
#        if self.sliceOrientation == 'y':
#            lev_bounds = [ 0, len( self.lev  ) * self.z_spacing ]
#            mapperBounds = [ bounds[0], bounds[1], sliceProperties - self.sliceThickness[1],  sliceProperties + self.sliceThickness[1], lev_bounds[0], lev_bounds[1]  ]
#        if self.sliceOrientation == 'z':
#            sliceThickness = self.z_spacing/2
#            mapperBounds = [ bounds[0], bounds[1], bounds[2], bounds[3], sliceProperties - sliceThickness,  sliceProperties + sliceThickness  ]
#        if mapperBounds:
#            print "Setting clip planes: %s " % str( mapperBounds )
#            self.clipBox.SetBounds( mapperBounds )
#            self.slice_filter.Modified()
##             self.clipper.PlaceWidget( mapperBounds )
##             self.clipper.GetPlanes( self.clippingPlanes )
##             self.mapper.SetClippingPlanes( self.clippingPlanes )
#            self.mapper.Modified()

     
    def CreateMap(self):        
        earth_source = vtk.vtkEarthSource()
        earth_source.SetRadius( self.earth_radius + .01 )
        earth_source.OutlineOn()
        earth_polydata = earth_source.GetOutput()
        self.earth_mapper = vtk.vtkPolyDataMapper()
        if vtk.VTK_MAJOR_VERSION <= 5:  self.earth_mapper.SetInput(earth_polydata)
        else:                           self.earth_mapper.SetInputData(earth_polydata)        
        self.earth_actor = vtk.vtkActor()
        self.earth_actor.SetMapper( self.earth_mapper )
        self.earth_actor.GetProperty().SetColor(0,0,0)
        self.renderer.AddActor( self.earth_actor )               
                
    def initCollections( self, nCollections, init_args, **args ):
        if nCollections > 0:
            args[ 'interactor' ] = self.renderWindowInteractor 
            args.update( self.plot_attributes )
            self.partitioned_point_cloud = vtkPartitionedPointCloud( nCollections, init_args, **args )
            self.partitioned_point_cloud.NewDataAvailable.connect( self.newDataAvailable )
            if not self.scaling_spec is None:
                print " initCollections.generateZScaling ", str( self.scaling_spec )
                self.partitioned_point_cloud.generateZScaling( spec=self.scaling_spec )

        else:
            self.render_mode = ProcessMode.LowRes
#        self.partitioned_point_cloud.connect( self.partitioned_point_cloud, QtCore.SIGNAL('updateScaling'), self.updateScaling )        
        self.createRenderer()
        self.low_res_actor = self.point_cloud_overview.actor
        self.renderer.AddActor( self.low_res_actor )
#        self.pointPicker.AddPickList( self.low_res_actor )
        
        if self.partitioned_point_cloud:
            for point_cloud in  self.partitioned_point_cloud.values():     
                self.renderer.AddActor( point_cloud.actor )
                self.pointPicker.AddPickList( point_cloud.actor )
        else:
            self.updateZRange( self.point_cloud_overview )
            
        self.mapManager = MapManager( roi = self.point_cloud_overview.getBounds() )
        self.renderer.AddActor( self.mapManager.getBaseMapActor() )
        self.renderer.AddActor( self.mapManager.getSphericalMap() )
        
    def reset( self, pcIndex ):
        if not self.isValid and ( self.partitioned_point_cloud <> None ):
            self.partitioned_point_cloud.clear( pcIndex )
            self.isValid = True
                    
    def updateZRange( self, pc ):
        nlev = pc.getNLevels()
        if nlev and (nlev <> self.nlevels):
            self.nlevels = nlev
            self.zSliceWidth = 1.0/(self.nlevels)
            self.sliceWidthSensitivity[2] = self.zSliceWidth
            self.slicePositionSensitivity[2] = self.zSliceWidth
            
            
    def refreshPointSize(self):
        self.point_cloud_overview.setPointSize( self.pointSize.getValue( ProcessMode.LowRes ) )
             
    def newDataAvailable( self, pcIndex, data_type ):
        if ( self.partitioned_point_cloud <> None ):
            interactionButtons = self.getInteractionButtons()
            scalarRange = interactionButtons.getConfigFunction('ScaleColormap').value 
            pc = self.partitioned_point_cloud.getPointCloud( pcIndex )
            pc.show()
#            self.decrementOverviewResolution()
            self.partitioned_point_cloud.postDataQueueEvent()
            pc.setScalarRange( scalarRange.getValues() )
            self.updateZRange( pc )
            trngs = pc.getThresholdingRanges() 
#            if trngs:
#                text = ' '.join( [ "%s: (%f, %f )" % (rng_val[0], rng_val[1], rng_val[2] )  for rng_val in trngs.values() ] )
#                text = " Thresholding Range[%d]: ( %.3f, %.3f )\n Colormap Range: %s " % ( pcIndex, trng[0], trng[1], str( self.scalarRange.getRange() ) )
#                self.updateTextDisplay( text )
#            print " Subproc[%d]-. new Thresholding Data Available: %s " % ( pcIndex, str( pc.getThresholdingRanges() ) ); sys.stdout.flush()
    #        self.reset( ) # pcIndex )
            self.render() 
                          
    def generateSubset(self, **args ):
#        self.pointPicker.GetPickList().RemoveAllItems() 
        self.getPointCloud().generateSubset( **args  )        
        
    def terminate(self):
        if ( self.partitioned_point_cloud <> None ):
            for point_cloud in self.partitioned_point_cloud.values(): 
                point_cloud.terminate()  
          
    def setPointSize( self, point_size ) :  
        self.getPointCloud().setPointSize( point_size ) 
        
    def getInitArgs(self, var1, var2, **args ):
        interface = None
        dfile = var1.parent
        data_file = dfile.id if dfile else None
        if data_file == None: data_file = self.plot_attributes.get( 'filename', None )
        if data_file == None: data_file = self.plot_attributes.get( 'url', None )
        varnames = [ getVarName( var1 ) ]
        if not var2 is None: varnames.append( getVarName( var2 ) )
        subSpace = args.get( 'axes', 'xyz' )
        grd_coords = [ None ]*5
        var_proc_op = None
        grid_file = None  
        ROI = None       
        return [ grid_file, data_file, interface, varnames, grd_coords, var_proc_op, ROI, subSpace ] 

    def gminit(self, var1, var2, **args  ):
        init_args = self.getInitArgs( var1, var2, **args )
        self.init( init=init_args, **args )

    def init(self, **args ):
        init_args = args.get( 'init', None )                  
        n_overview_points = args.get( 'n_overview_points', 500000 )    
        n_subproc_points = args.get( 'n_subproc_points', 500000 )  
        n_cores = args.get( 'n_cores', multiprocessing.cpu_count() )    
        self.point_cloud_overview = vtkLocalPointCloud( 0, max_points=n_overview_points ) 
        lut = self.getLUT()
        self.point_cloud_overview.initialize( init_args, lut = lut, maxStageHeight=self.maxStageHeight  )
        nInputPoints = self.point_cloud_overview.getNumberOfInputPoints()
        if ( n_subproc_points > nInputPoints ): n_subproc_points = nInputPoints
        nPartitions = int( round( min( nInputPoints / n_subproc_points, 10  ) ) )
        nCollections = min( nPartitions, n_cores-1 )
        print " Init PCViewer, nInputPoints = %d, n_overview_points = %d, n_subproc_points = %d, nCollections = %d, overview skip index = %s, init_args = %s" % ( nInputPoints, n_overview_points, n_subproc_points, nCollections, self.point_cloud_overview.getSkipIndex(), str( init_args ) )
        self.initCollections( nCollections, init_args, lut = lut, maxStageHeight=self.maxStageHeight  )
        self.defvar =  init_args[3]
        self.vertVar = None
        self.initializeConfiguration()       
        self.buttonBarHandler.cfgManager.initParameters()
        self.initializePlots()
        self.setCameraPos()
             
#             pc = self.point_cloud_overview.getPointCollection()
#             cfgInterface = ConfigurationInterface( metadata=pc.getMetadata(), defvar=pc.var.id, callback=self.processConfigCmd  )
#             cfgInterface.build()
#             cfgInterface.activate()
            
        self.start()

    def initializePlots(self):            
        DV3DPlot.initializePlots(self)
        self.updateVerticalScaling() 
#        self.setRenderMode( ProcessMode.HighRes )                      
                   
    def setCameraPos(self):
        ( xcenter, ycenter, xwidth, ywidth ) = self.point_cloud_overview.getCenter()
        self.initCamera( ( xwidth + ywidth ), ( xcenter, ycenter ) )

    def initializeConfiguration( self, cmap_index=0, **args ):
#        ispec = self.inputSpecs[ cmap_index ] 
#        args['units'] = ispec.units
        self.buttonBarHandler.initializeConfigurations( **args )

    
class QPointCollectionMgrThread( threading.Thread ):
    
    def __init__( self, pointCollectionMgr, **args ):
        threading.Thread.__init__( self ) # , parent=pointCollectionMgr )
        self.pointCollectionMgr = pointCollectionMgr
        self.delayTime = args.get( 'delayTime', 0.02 )
        self.args = args
        
    def init(self):
        self.pointCollectionMgr.init( **self.args )
         
    def run(self):
        while self.pointCollectionMgr.running:
            self.pointCollectionMgr.update()
            time.sleep( self.delayTime )
        self.exit(0)       
                             
# if __name__ == '__main__':
#     import argparse
#     parser = argparse.ArgumentParser(description='DV3D Point Cloud Viewer')
#     parser.add_argument( 'PATH' )
#     parser.add_argument( '-d', '--data_dir', dest='data_dir', nargs='?', default="~/data", help='input data dir')
#     parser.add_argument( '-t', '--data_type', dest='data_type', nargs='?', default="CSU", help='input data type')
#     ns = parser.parse_args( sys.argv )
#     
#     kill_all_zombies()
# 
#     app = QtGui.QApplication(['Point Cloud Plotter'])
#     point_size = 1
#     n_overview_points = 500000
#     height_varname = None
#     data_dir = os.path.expanduser( ns.data_dir )
#     height_varnames = []
#     var_proc_op = None
#     
#     if ns.data_type == "WRF":
#         data_file = os.path.join( data_dir, "WRF/wrfout_d01_2013-07-01_00-00-00.nc" )
#         grid_file = None
#         varname = "U"        
#     elif ns.data_type == "CAM":
#         data_file = os.path.join( data_dir, "CAM/f1850c5_t2_ANN_climo-native.nc" )
#         grid_file = os.path.join( data_dir, "CAM/ne120np4_latlon.nc" )
#         varname = "U"
#         height_varnames = [ "Z3" ]
#     elif ns.data_type == "ECMWF":
#         data_file = os.path.join( data_dir, "AConaty/comp-ECMWF/ecmwf.xml" )
#         grid_file = None
#         varname = "U_velocity"   
#     elif ns.data_type == "GEOS5":
#         data_file = os.path.join( data_dir, "AConaty/comp-ECMWF/ac-comp1-geos5.xml" )
#         grid_file = None
#         varname = "uwnd"   
#     elif ns.data_type == "MMF":
#         data_file = os.path.join( data_dir, "MMF/diag_prs.20080101.nc" )
#         grid_file = None
#         varname = "u"
#     elif ns.data_type == "GEOD":
#         file_name =  "temperature_19010101_000000.nc" # "vorticity_19010102_000000.nc" # 
#         data_file = os.path.join( data_dir, "GeodesicGrid", file_name )
#         grid_file = os.path.join( data_dir, "GeodesicGrid", "grid.nc" )
#         varname = "temperature_ifc" # "vorticity" # 
#         var_proc_op = None
#     elif ns.data_type == "CSU":
#         file_name =  "psfc.nc" 
#         data_file = os.path.join( data_dir, "ColoState", file_name )
#         grid_file = os.path.join( data_dir, "ColoState", "grid.nc" )
#         varname = "pressure" 
#         var_proc_op = None
#         
#     g = CPCPlot() 
#     g.init( init_args = ( grid_file, data_file, varname, height_varname, var_proc_op ), n_overview_points=n_overview_points, n_cores=2 ) # , n_subproc_points=100000000 )
#     g.createConfigDialog()
#         
#     app.connect( app, QtCore.SIGNAL("aboutToQuit()"), g.terminate ) 
#     app.exec_() 
#     g.terminate() 
#     
