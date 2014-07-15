'''
Created on Apr 29, 2014

@author: tpmaxwel
'''
    
import sys, vtk, cdms2, traceback, os, cdtime, math 
from ColorMapManager import *  
from Shapefile import shapeFileReader   
from ImagePlaneWidget import ImagePlaneWidget  
from DistributedPointCollections import kill_all_zombies
from StructuredGridPlot import  *
from StructuredDataset import *
import numpy as np

LegacyAbsValueTransferFunction = 0
LinearTransferFunction = 1 
PosValueTransferFunction = 2  
NegValueTransferFunction = 3  
AbsValueTransferFunction = 4

PositiveValues = 0
NegativeValues = 1
AllValues = 2
    
class TransferFunction:
    
    def __init__(self, tf_type, **args ):
        self.type = tf_type
        self.data = args.get( 'data', None )
                
    def setType(self, tf_type ):
        self.type = tf_type
        
class NodeData:
    RED = 0
    BLUE = 1
    YELLOW = 2
    CYAN = 3
    MAGENTA = 4
    GRAY = 5
    
    def __init__(self, **args ):
        self.ix0 = args.get( "ix0", None )
        self.y0 = args.get( "y0", None )
        self.ix1 = args.get( "ix1", None )
        self.y1 = args.get( "y1", None )
        self.s = args.get( "s", 0.5 )
        self.color = args.get( "color", NodeData.YELLOW )
        self.free = args.get( "free", False )
        self.index =  args.get( "index", -1 )
        self.dx0 = args.get( "dx0", None )
        self.dx1 = args.get( "dx1", None )
        self.spt0 = None
        self.spt1 = None
        self.vector = None

    def setImageVectorData(self, ipt1, s ): 
        self.ix1 = ipt1[0] 
        self.y1 = ipt1[1]  
        self.s = s 

    def getDataPoint(self):
        return ( self.dx0, self.y0 )

    def getDataEndPoint(self):
        return ( self.dx1, self.y1 ) if self.dx1 else None
    
    def getDataPosition( self ): 
        if self.dx1 == None: return [ self.dx0, self.y0 ]
        return [ self.dx0 + self.s * ( self.dx1 - self.dx0 ), self.y0 + self.s * ( self.y1 - self.y0 ) ]
        
    def getImagePosition( self ): 
        if self.ix1 == None: return [ self.ix0, self.y0 ]
        return [ self.ix0 + self.s * ( self.ix1 - self.ix0 ), self.y0 + self.s * ( self.y1 - self.y0 ) ]
    
    def getScenePoint(self):
        return self.spt0

    def getSceneEndPoint(self):
        return self.spt1
         
class RectGridPlot(StructuredGridPlot): 

    NI_RANGE_POSITION = 10001
    NI_RANGE_WIDTH = 10002
    NI_SHAPE_ADJ_0 = 10003
    NI_SHAPE_ADJ_1 = 10004

    global_coords = [-1, -1, -1]

    NoButtonDown = 0
    RightButtonDown = 1
    LeftButtonDown = 2

    Start = 0
    Cursoring = 1
    Outside  = 2
    
    def __init__( self, **args ):
        StructuredGridPlot.__init__( self, **args  )
#         self.addUVCDATConfigGuiFunction( 'contourColormap', ColormapConfigurationDialog, 'K', label='Choose Contour Colormap', setValue=lambda data: self.setColormap(data,1) , getValue=lambda: self.getColormap(1), layerDependent=True, isValid=self.hasContours, group=ConfigGroup.Color )
        self.sliceOutputShape = args.get( 'slice_shape', [ 100, 50 ] )
        self.polygonActor = None
        self.opacity = [ 1.0, 1.0 ]
        self.updatingPlacement = False
        self.isSlicing = False
        self.planeWidgetX = None
        self.planeWidgetY = None
        self.planeWidgetZ = None
        self.opacityUpdateCount = 0
        self.generateContours = False
        self.contourLineActors = {}
        self.contourLineMapperer = None
        self.contours = None
        self.NumContours = 10.0
        self.showOutlineMap = True
        self.zincSkipIndex = 1
        self.state = self.Start
        self.volume = None
        self.probeFilter = None
        self.cursorActor     = vtk.vtkActor()
        
        self.levelSetActor = None
        self.surfacePicker = None

        self.max_opacity = 1.0
        self.vthresh = None
        self.filterOutliers = False
        self.refinement = [ 0.0, 0.5 ]
        self.otf_data = None
        self.ctf_data = None
        self.updatingOTF = False
        self.configTime = None
        self.clipping_enabled = False
        self.cropRegion = None
        self.cropZextent = None
        self.clipper = None
        self.volRenderConfig = [ 'Default', 'False' ]
        self.transFunctGraphVisible = False
        self.transferFunctionConfig = None
        self.volumeMapper = None
        interactionButtons = self.getInteractionButtons()
        interactionButtons.addSliderButton( names=['ScaleColormap'], key='C', toggle=True, label='Colormap Scale', interactionHandler=self.processColorScaleCommand )
        interactionButtons.addSliderButton( names=['ScaleTransferFunction'], key='T', toggle=True, parents=['ToggleVolumePlot'], label='Transfer Function Range', interactionHandler=self.processThresholdRangeCommand )
        interactionButtons.addSliderButton( names=['ScaleOpacity'], key='o', toggle=True, label='Opacity Scale', range_bounds=[ 0.0, 1.0 ], initValue=[ 1.0, 1.0 ], interactionHandler=self.processOpacityScalingCommand )
        interactionButtons.addSliderButton( names=['IsosurfaceValue'], key='L', toggle=True, parents=['ToggleSurfacePlot'], sliderLabels='Isosurface Value', label='Positioning Isosurface', interactionHandler=self.processIsosurfaceValueCommand )
        self.fetchPlotButtons()
        
#         self.addConfigurableLevelingFunction( 'colorScale', 'C', label='Colormap Scale', units='data', setLevel=self.scaleColormap, getLevel=self.getDataRangeBounds, layerDependent=True, adjustRangeInput=0, group=ConfigGroup.Color )
#         self.addConfigurableLevelingFunction( 'opacity', 'O', label='Slice Plane Opacity', rangeBounds=[ 0.0, 1.0 ],  setLevel=self.setOpacity, activeBound='min',  getLevel=self.getOpacity, isDataValue=False, layerDependent=True, bound = False, group=ConfigGroup.Rendering )
#         self.addConfigurableLevelingFunction( 'contourDensity', 'g', label='Contour Density', activeBound='max', setLevel=self.setContourDensity, getLevel=self.getContourDensity, layerDependent=True, windowing=False, rangeBounds=[ 3.0, 30.0, 1 ], bound=False, isValid=self.hasContours, group=ConfigGroup.Rendering )
#         self.addConfigurableLevelingFunction( 'contourColorScale', 'S', label='Contour Colormap Scale', units='data', setLevel=self.scaleContourColormap, getLevel=lambda:self.getDataRangeBounds(1), layerDependent=True, adjustRangeInput=1, isValid=self.hasContours, group=ConfigGroup.Color )
#         self.addConfigurableLevelingFunction( 'coastlines_Line', 'm0', label='Coastline Line', setLevel=self.setBasemapCoastlineLineSpecs, getLevel=self.getBasemapCoastlineLineSpecs, sliderLabels=[ 'Thickness', 'Density' ], layerDependent=False, rangeBounds=[ 0.0, 3.49 ], initRange=[ 1.0, 1.0, 1 ], group=ConfigGroup.BaseMap )
#         self.addConfigurableLevelingFunction( 'countries_Line', 'm1', label='Countries Line', setLevel=self.setBasemapCountriesLineSpecs, getLevel=self.getBasemapCountriesLineSpecs, sliderLabels=[ 'Thickness', 'Density' ], layerDependent=False, rangeBounds=[ 0.0, 3.49 ], initRange=[ 0.0, 1.0, 0 ], group=ConfigGroup.BaseMap )
#         self.addConfigurableLevelingFunction( 'states_Line', 'm2', label='States Line', setLevel=self.setBasemapStatesLineSpecs, getLevel=self.getBasemapStatesLineSpecs, sliderLabels=[ 'Thickness', 'Density' ], layerDependent=False, rangeBounds=[ 0.0, 3.49 ], initRange=[ 0.0, 1.0, 0 ], group=ConfigGroup.BaseMap )
#         self.addConfigurableLevelingFunction( 'lakes_Line', 'm3', label='Lakes Line', setLevel=self.setBasemapLakesLineSpecs, getLevel=self.getBasemapLakesLineSpecs, sliderLabels=[ 'Thickness', 'Density' ], layerDependent=False, rangeBounds=[ 0.0, 3.49 ], initRange=[ 0.0, 1.0, 0 ], group=ConfigGroup.BaseMap )
#        self.setupTransferFunctionConfigDialog()
#       self.addConfigurableLevelingFunction( 'colorScale',    'C', label='Colormap Scale', units='data', setLevel=self.generateCTF, getLevel=self.getSgnRangeBounds, layerDependent=True, adjustRangeInput=0, group=ConfigGroup.Color )
#        self.addConfigurableLevelingFunction( 'functionScale', 'T', label='Transfer Function Scale', units='data', setLevel=self.generateOTF, getLevel=self.getAbsRangeBounds, layerDependent=True, adjustRangeInput=0, initRefinement=[ self.refinement[0], self.refinement[1] ], gui=self.transferFunctionConfig, group=ConfigGroup.Rendering  )
#        self.addConfigurableLevelingFunction( 'opacityScale',  'o', label='Transfer Function Opacity', setLevel=self.adjustOpacity, layerDependent=True, group=ConfigGroup.Rendering  )
#        self.addConfigurableMethod( 'showTransFunctGraph', self.showTransFunctGraph, 'g', label='Transfer Function Graph', group=ConfigGroup.Rendering )
#        self.addConfigurableBooleanFunction( 'cropRegion', self.toggleClipping, 'X', labels='Start Cropping|End Cropping', signature=[ ( Float, 'xmin'), ( Float, 'xmax'), ( Float, 'ymin'), ( Float, 'ymax'), ( Float, 'zmin'), ( Float, 'zmax') ], group=ConfigGroup.Display )
#        self.addConfigurableLevelingFunction( 'zScale', 'z', label='Vertical Scale', setLevel=self.setInputZScale, activeBound='max', getLevel=self.getScaleBounds, windowing=False, sensitivity=(10.0,10.0), initRange=[ 2.0, 2.0, 1 ], group=ConfigGroup.Display )
#        self.addUVCDATConfigGuiFunction( 'renderType', VolumeRenderCfgDialog, 'v', label='Choose Volume Renderer', setValue=self.setVolRenderCfg, getValue=self.getVolRenderCfg, layerDependent=True, group=ConfigGroup.Rendering )


            
    def processOpacityScalingCommand( self, args, config_function = None ):
        opacityRange = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            if config_function.initial_value == None:       
                config_function.initial_value = [ 1.0, 1.0 ]  
            self.setOpacity( config_function.initial_value )
            self.adjustOpacity( config_function.initial_value )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
            bbar = self.getInteractionButtons()
            bbar.slidersVisible = [ ( islider < len(config_function.sliderLabels) ) for islider in range(4)]
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            val = args[2].GetValue()     
            opacityRange.setValue( args[1], val )
            orange = opacityRange.getValues()
            self.setOpacity( orange )
            self.adjustOpacity( orange )

    def processColorScaleCommand( self, args, config_function = None ):
        colorScaleRange = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            init_range = self.getDataRangeBounds()
            config_function.setRangeBounds( init_range ) 
            if config_function.initial_value == None:       
                config_function.initial_value = init_range  
            self.scaleColormap( config_function.initial_value )
            self.generateCTF( config_function.initial_value )
            colorScaleRange.setValues( init_range )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
            bbar = self.getInteractionButtons()
            bbar.slidersVisible = [ ( islider < len(config_function.sliderLabels) ) for islider in range(4)]
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            value = args[2].GetValue() 
            colorScaleRange.setValue( args[1], value )
            cscale = colorScaleRange.getValues()
            self.scaleColormap( cscale )
            self.generateCTF( cscale )
            
    def setIsosurfaceLevel( self, value ):
        if self.levelSetActor <> None:
            self.levelSetFilter.SetValue ( 0, value ) 
            self.levelSetFilter.Modified()
                 
    def processIsosurfaceValueCommand( self, args, config_function = None ):
        isosurfaceValue = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            init_range = self.getSgnRangeBounds()
            config_function.setRangeBounds( init_range )
            if config_function.initial_value == None:
                init_value = (init_range[0]+init_range[1])/2.0          
                config_function.initial_value = init_value             
            self.setIsosurfaceLevel( config_function.initial_value ) 
            isosurfaceValue.setValues( [ config_function.initial_value ] )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
            bbar = self.getInteractionButtons()
            bbar.slidersVisible = [ ( islider < len(config_function.sliderLabels) ) for islider in range(4)]
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            value = args[2].GetValue()
            isosurfaceValue.setValue( 0, value )
            self.setIsosurfaceLevel( value ) 
 
    def processThresholdRangeCommand( self, args, config_function = None ):
        volumeThresholdRange = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            init_range = self.getSgnRangeBounds()
            config_function.setRangeBounds( init_range )
            if config_function.initial_value == None:      
                config_function.initial_value = init_range   
            self.generateOTF( config_function.initial_value )
            volumeThresholdRange.setValues( config_function.initial_value )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
            bbar = self.getInteractionButtons()
            bbar.slidersVisible = [ ( islider < len(config_function.sliderLabels) ) for islider in range(4)]
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            vt_range = self.getSgnRangeBounds()
            value = args[2].GetValue()
            vt_range[ args[1] ] = value
            self.generateOTF( vt_range )
            volumeThresholdRange.setValues( vt_range )

    def processSlicingCommand( self, args, config_function = None ):
        plane_index, plane_widget = self.getPlaneWidget( config_function.key )
        slicePosition = config_function.value
#        print " ProcessSlicingCommand: args = %s, plane = %d, cf = %s" % ( str( args ), plane_index, config_function.key )
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            primaryInput = self.input()
            bounds = list( primaryInput.GetBounds() ) 
            init_range = [ bounds[2*plane_index], bounds[2*plane_index+1] ]
            config_function.setRangeBounds( init_range ) 
            if config_function.initial_value == None:
                config_function.initial_value = init_range
            slicePosition.setValues( [ config_function.initial_value[0] ] ) 
            plane_widget.SetSlicePosition( config_function.initial_value[0] )
            if config_function.key == 'z':
                self.ProcessIPWAction( plane_widget, ImagePlaneWidget.InteractionUpdateEvent, action = ImagePlaneWidget.Pushing )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            if (len(args) > 2) and args[2]: 
                for index in range(3):  self.modifySlicePlaneVisibility( index, "xyz"[index], False ) 
                self.updateTextDisplay( config_function.label ) 
            self.modifySlicePlaneVisibility( plane_index, config_function.key, args[1] )
            self.render() 
        elif args and args[0] == "ProcessSliderInit":
            for plane_index in range(3):
                self.modifySlicePlaneVisibility( plane_index, 'xyz'[plane_index]  ) 
            self.render()              
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            value = args[2].GetValue()
            plane_widget.SetSlicePosition( value )
            slicePosition.setValues( [ value ] )
            self.ProcessIPWAction( plane_widget, ImagePlaneWidget.InteractionUpdateEvent, action = ImagePlaneWidget.Pushing )

 
    def resetCamera(self, **args):
        self.cropRegion = self.getVolumeBounds()
        self.cropZextent = None
        self.cropVolume( False ) 
        self.render()

    def processScaleChange( self, old_spacing, new_spacing ):
        if self.cropRegion:
            if self.clipping_enabled: self.toggleClipping()
            extent = self.cropZextent if self.cropZextent else self.input().GetExtent()[4:6] 
            origin = self.input().GetOrigin() 
            for ib in [4,5]: 
                self.cropRegion[ib] = ( origin[int(ib/2)] + new_spacing[int(ib/2)]*extent[ib-4] ) 
            if (self.volumeMapper <> None) and self.volumeMapper.GetCropping():
                self.cropVolume( False )                 
        if self.planeWidgetZ.IsVisible():      
            self.planeWidgetZ.UpdateInputs()
         
    def activateEvent( self, caller, event ):
        StructuredGridPlot.activateEvent( self, caller, event )
        if self.clipper and ( self.cropRegion == None ):
            self.renwin = self.renderer.GetRenderWindow( )
            if self.renwin <> None:                
                if ( self.renderWindowInteractor <> None ): 
                    self.clipper.SetInteractor( self.renderWindowInteractor )
                    self.cropRegion = self.getVolumeBounds()
                    self.clipper.PlaceWidget( self.cropRegion )
                    self.clipPlanes = vtk.vtkPlanes() 
                    self.clipper.GetPlanes( self.clipPlanes )
        self.render() 

    def getVolumeBounds( self, **args ):  
        extent = args.get( "extent", self.input().GetExtent() )
        spacing = args.get( "spacing", self.input().GetSpacing() )
        origin = args.get( "origin", self.input().GetOrigin() )
        bounds = [0]*6
        for i in range(6):
            io2 = int(i/2)
            bounds[i] = origin[io2] + spacing[io2]*extent[i] 
        return bounds
                 
    def clipOn(self):
        if self.cropRegion == None: self.cropRegion = self.getVolumeBounds()
        self.clipper.PlaceWidget( self.cropRegion )
        self.clipper.SetHandleSize( 0.005 )
        self.clipper.SetEnabled( True )
        self.clipper.On()
        self.executeClip()

    def clipOff(self):
        self.clipper.SetEnabled( False )
        self.clipper.Off()
                   
    def toggleClipping( self, enableClipping ):
        self.clipping_enabled = enableClipping        
        if self.clipping_enabled:
            self.volumeMapper.CroppingOn()     
            self.clipOn()
        else:                            
            self.clipOff()
    
    def getVolRenderCfg( self ):
        return [ ';'.join( self.volRenderConfig ) ]

    def setVolRenderCfg( self, config_str, doRender = True ):
        if config_str: 
            self.volRenderConfig = getItem( config_str ).strip('[]').split(';')
        renderMode = vtk.vtkSmartVolumeMapper.TextureRenderMode
        if self.volRenderConfig[0] == 'RayCastAndTexture': 
            renderMode = vtk.vtkSmartVolumeMapper.RayCastAndTextureRenderMode
        elif self.volRenderConfig[0] == 'RayCast': 
            renderMode = vtk.vtkSmartVolumeMapper.RayCastRenderMode
        elif self.volRenderConfig[0] == 'Texture3D': 
            renderMode = vtk.vtkSmartVolumeMapper.TextureRenderMode            
        self.volumeMapper.SetRequestedRenderMode( renderMode )
        self.volumeProperty.SetShade( self.volRenderConfig[1] == str(True) )             
        if doRender: self.render() 

    def getZScale( self ):
        if self.volume <> None:
            spacing = self.volume.GetScale()
            sx, sy, sz = spacing    
            return [ 1.0, sz, 1 ]  
    
    def configTransferFunction(self, nodeIndex, value0, value1, value2 ):
        rangeBounds = self.getRangeBounds()
        if nodeIndex == self.NI_RANGE_POSITION:                   
            self.max_opacity = value1
            range_size = ( rangeBounds[1] - rangeBounds[0])  
            new_peak = bound( self.getImageValue( value0 ), [ rangeBounds[0] + 0.01*range_size, rangeBounds[0] + 0.99*range_size ] )
            w = ( self._range[1] - self._range[0]  ) / 2.0
            self._range[0] = new_peak - w
            self._range[1] = new_peak + w
            if self._range[0] < rangeBounds[0]:
                self._range[0] = rangeBounds[0]
                if 2.0*new_peak < range_size: 
                    self._range[1] = rangeBounds[0] + 2.0*new_peak
                else: 
                    self._range[1] = rangeBounds[1]
            if self._range[1] > rangeBounds[1]:
                self._range[1] = rangeBounds[1]              
                self._range[0] = rangeBounds[1] - 2.0*( rangeBounds[1] - new_peak )
                if self._range[0] < rangeBounds[0]:  self._range[0] = rangeBounds[0]
#            print " config RANGE_POSITION: ", nodeIndex, self.max_opacity, new_peak, str( self._range ) 
        elif nodeIndex == self.NI_RANGE_WIDTH:
            self._range[1] = self.getImageValue( value0 )
            if self._range[1] > rangeBounds[1]: self._range[1] = rangeBounds[1] 
#            print " config RANGE_WIDTH: ", nodeIndex, value0, self._range[1] 
        elif nodeIndex == self.NI_SHAPE_ADJ_0:
            self.refinement[0] = value2
#            print " config SHAPE_ADJ_0: ", nodeIndex, value2
        elif nodeIndex == self.NI_SHAPE_ADJ_1:
            self.refinement[1] = value2
#            print " config SHAPE_ADJ_1: ", nodeIndex, value2
        else: return
        
        self.updateOTF()
        self.render()

    def getAbsRangeBounds(self): 
        full_range = self.getDataRangeBounds()
        abs_range = []
        abs_range.append( max( 0.0, full_range[0]) )
        abs_range.append( max( abs(full_range[1]), abs(full_range[0]) ) )
#        abs_range.append( self.transferFunctionConfig.getTransferFunctionType() if self.transferFunctionConfig else AbsValueTransferFunction )
        abs_range.append(  AbsValueTransferFunction )
        return abs_range

    def getSgnRangeBounds(self): 
        range = self.getDataRangeBounds()
        if self.transferFunctionConfig:
            range[2] = self.transferFunctionConfig.getTransferFunctionType()
        return range
                
    def persistTransferFunctionConfig( self ):
        parmList = []
        cfs = []
        configFunct = self.configurableFunctions[ 'opacityScale' ]
        opacity_value = [ 0.0, self.max_opacity ] 
        for i in range( 2 ): configFunct.range[i] = opacity_value[i]
        parmList.append( ('opacityScale', opacity_value ) )
        cfs.append( configFunct )

        configFunct = self.configurableFunctions[ 'functionScale' ]
        new_values = self.getDataValues( self._range[0:2] )
        range_values = configFunct.range
        print "Update Range Values:  %s -> %s, max_opacity = %.2f " % ( str( range_values ), str( new_values ), self.max_opacity )
        for i in range( 2 ): range_values[i] = new_values[i]
        for i in range( 2 ): range_values[i+3] = self.refinement[i]
        range_values[2] = AbsValueTransferFunction # self.transferFunctionConfig.getTransferFunctionType()
        parmList.append( ('functionScale', range_values ) )
        cfs.append( configFunct )
               
        self.persistParameterList( parmList )
#        for configFunct in cfs: configFunct.initLeveling()

    def persistCropRegion( self ):
        if self.cropRegion:
            parmList = []
            parmList.append( ( 'cropRegion', self.cropRegion ) ) 
            self.persistParameterList( parmList )
            
    def persistParameterList( self, parmList ):
        pass
        
    def clearTransferFunctionConfigDialog(self):
        self.persistTransferFunctionConfig()
        self.transFunctGraphVisible = False
        self.resetNavigation()

    def showTransFunctGraph( self ): 
        self.transFunctGraphVisible = True
        self.updateOTF()
                 
    def onRender( self, caller, event ):
        pass
#        scale = self.volume.GetScale()
#        bounds = self.volume.GetBounds()
#        origin = self.volume.GetOrigin()
#        dims = [  int( round( ( bounds[2*i+1]-bounds[2*i] ) / scale[i] ) ) for i in range(3) ]
#        print " Volume position: %s " % str( self.volume.GetPosition() )
#        print "Volume Render Event: scale = %s, bounds = %s, origin = %s, dims = %s " % ( str2f( scale ), str2f( bounds ), str2f( origin ), str( dims )  )

    def doPick( self, X, Y ):  
        found = 0
        if self.surfacePicker:
            self.surfacePicker.Pick( X, Y, 0.0, self.renderer )
            path = self.surfacePicker.GetPath()        
            if path:
                path.InitTraversal()
                for _ in range( path.GetNumberOfItems() ):
                    node = path.GetNextNode()
                    if node and ( node.GetViewProp() == self.levelSetActor ):
                        found = 1
                        break
        return found

    def onLeftButtonPress( self, caller, event ):        
        if False: # self.surfacePicker and not shift: 
            if self.state == self.Outside:
                self.stopCursor()
            elif self.state == self.Start:
                X = self.renderWindowInteractor.GetEventPosition()[0]
                Y = self.renderWindowInteractor.GetEventPosition()[1]       
                if self.doPick( X, Y ): 
                    self.startCursor() 
        else:           
            StructuredGridPlot.onLeftButtonPress( self, caller, event )

#    def onLeftButtonRelease( self, caller, event ):
#        if self.state == self.Cursoring:
#            self.stopCursor()
#        else:
#            StructuredGridPlot.onLeftButtonRelease( self, caller, event )

    def onModified(self, caller, event ):
        
        if self.state == self.Cursoring:
            X = self.renderWindowInteractor.GetEventPosition()[0]
            Y = self.renderWindowInteractor.GetEventPosition()[1]
                    
            camera = self.renderer.GetActiveCamera()
            if (  not camera ): return
                                        
            self.updateCursor(X,Y)         
            self.renderWindowInteractor.Render()
        else:
            StructuredGridPlot.onModified( self, caller, event ) 
        
    def startCursor(self):
        if self.state == self.Cursoring: return
         
    
        X = self.renderWindowInteractor.GetEventPosition()[0]
        Y = self.renderWindowInteractor.GetEventPosition()[1]
        
        # Okay, make sure that the pick is in the current renderer
        if ( not self.renderer or  not self.renderer.IsInViewport(X, Y)):        
            self.state  = self.Outside
            return
        
        if self.doPick( X, Y ):      
            self.state  = self.Cursoring
            self.haltNavigationInteraction()
            self.cursorActor.VisibilityOn()
            self.updateCursor(X,Y)
#            self.startInteraction()
#            self.processEvent( self.InteractionStartEvent )
            self.renderWindowInteractor.Render()  
            return 1     
        else:
            self.state  = self.Outside
            self.cursorActor.VisibilityOff()
            return 0            

    def stopCursor(self):                            
#        self.ProcessEvent( self.InteractionEndEvent )
        self.state  = self.Start
        self.cursorActor.VisibilityOff() 
        self.resetNavigationInteraction()     
#        self.endInteraction()
        self.renderWindowInteractor.Render()
        
    def updateCursor( self, X, Y ):
        if self.surfacePicker:        
            self.surfacePicker.Pick( X, Y, 0.0, self.renderer )            
            if self.doPick( X, Y ):    
                self.cursorActor.VisibilityOn()
            else:
                self.cursorActor.VisibilityOff()
                return 
                                         
#            pos = self.surfacePicker.GetPickPosition()    
#            if( pos == None ):        
#                self.cursorActor.VisibilityOff()
#                return
#            self.cursor.SetCenter ( pos[0], pos[1], pos[2] )
            self.displayPickData()
            
    def displayPickData( self ):
        pointId =  self.surfacePicker.GetPointId() 
        if pointId < 0:
            self.cursorActor.VisibilityOff()
        else:
            level_ispec = self.getInputSpec() 
            if level_ispec and level_ispec.input(): 
                pdata = self.levelSetFilter.GetOutput()
                point_data = pdata.GetPointData()
                pos = pdata.GetPoint( pointId )
                self.cursor.SetCenter ( pos[0], pos[1], pos[2] )
                scalarsArray = point_data.GetScalars()
                image_data_value = scalarsArray.GetTuple1( pointId )
                data_value = level_ispec.getDataValue( image_data_value )
                textDisplay = " Position: (%.2f, %.2f, %.2f), Level Value: %.3G %s" % ( pos[0], pos[1], pos[2], data_value, level_ispec.units )  
                texture_ispec = self.getInputSpec(  1 )                
                if texture_ispec and texture_ispec.input():
                    tex_pdata = self.probeFilter.GetOutput()
                    tex_point_data = tex_pdata.GetPointData()
                    tex_scalarsArray = tex_point_data.GetScalars()
                    tex_image_data_value = tex_scalarsArray.GetTuple1( pointId )
                    tex_data_value = texture_ispec.getDataValue( tex_image_data_value )
                    textDisplay += ", Texture value: %.3G %s" % ( tex_data_value, texture_ispec.units )
                self.updateTextDisplay( textDisplay )                        

    def enableDualInputs(self):
        if self.levelSetActor <> None:
            if self.probeFilter == None:
                ispec = self.getInputSpec( 1 )   
                self.probeFilter = vtk.vtkProbeFilter()
                textureRange = ispec.GetScalarRange()
                self.probeFilter.SetSource( ispec )
                self.generateTexture = True
                mapperInputPort = self.levelSetFilter.GetOutputPort() # self.polyClipper.GetOutputPort() 
                self.probeFilter.SetInputConnection( mapperInputPort )
                self.levelSetMapper.SetInputConnection( self.probeFilter.GetOutputPort() ) 
                self.levelSetMapper.SetScalarRange( textureRange )


    def buildIsosurfacePipeline(self):
        """ execute() -> None
        Dispatch the vtkRenderer to the actual rendering widget
        """ 
        
        texture_ispec = self.getInputSpec( 1 )                
        xMin, xMax, yMin, yMax, zMin, zMax = self.input().GetExtent()       
        self.sliceCenter = [ (xMax-xMin)/2, (yMax-yMin)/2, (zMax-zMin)/2  ]       
        spacing = self.input().GetSpacing()
        sx, sy, sz = spacing 
#        self.input().SetSpacing( sx, sy, 5*sz )      
        origin = self.input().GetOrigin()
        ox, oy, oz = origin
        dataType = self.input().GetScalarTypeAsString()
        self.setMaxScalarValue( self.input().GetScalarType() )
        self.colorByMappedScalars = False
        rangeBounds = self.getRangeBounds()

        dr = rangeBounds[1] - rangeBounds[0]
        range_offset = .2*dr
        self.range = [ rangeBounds[0] + range_offset, rangeBounds[1] - range_offset ]
        print "Data Type = %s, range = (%f,%f), range bounds = (%f,%f), max_scalar = %s" % ( dataType, self.range[0], self.range[1], rangeBounds[0], rangeBounds[1], self._max_scalar_value )
        self.probeFilter = None
        textureRange = self.range
        if texture_ispec and texture_ispec.input():
            self.probeFilter = vtk.vtkProbeFilter()
            textureRange = texture_ispec.input().GetScalarRange()
            if vtk.VTK_MAJOR_VERSION <= 5:  self.probeFilter.SetSource( texture_ispec.input() )
            else:                           self.probeFilter.SetSourceData( texture_ispec.input() )
            self.generateTexture = True

        if (self.surfacePicker == None):           
            self.surfacePicker  = vtk.vtkPointPicker()
                    
        self.levelSetFilter = vtk.vtkContourFilter()
        
        if vtk.VTK_MAJOR_VERSION <= 5:  self.levelSetFilter.SetInput(self.input())
        else:                           self.levelSetFilter.SetInputData(self.input())        

#         self.clipper.GetPlanes( self.clipPlanes )
#         self.polyClipper = vtk.vtkClipPolyData()
#         self.polyClipper.SetInputConnection( self.levelSetFilter.GetOutputPort() )
#         self.polyClipper.SetClipFunction( self.clipPlanes )
#         self.polyClipper.InsideOutOn()
#         self.polyClipper.GenerateClipScalarsOn()     
                
        self.levelSetMapper = vtk.vtkPolyDataMapper()
        self.levelSetMapper.SetColorModeToMapScalars()
        mapperInputPort = self.levelSetFilter.GetOutputPort() # self.polyClipper.GetOutputPort() 
        if ( self.probeFilter == None ):
            imageRange = self.getImageValues( self.range ) 
            self.levelSetMapper.SetInputConnection( mapperInputPort ) 
            self.levelSetMapper.SetScalarRange( imageRange[0], imageRange[1] )
        else: 
            self.probeFilter.SetInputConnection( mapperInputPort )
            self.levelSetMapper.SetInputConnection( self.probeFilter.GetOutputPort() ) 
            self.levelSetMapper.SetScalarRange( textureRange )
            
        colormapManager = self.getColormapManager( index=1 ) if texture_ispec and texture_ispec.input() else self.getColormapManager() 
        self.scaleColormap( textureRange, 1 )                 
#        colormapManager = self.getColormapManager()                  
        colormapManager.setAlphaRange ( [ 1, 1 ] ) 
        self.levelSetMapper.SetLookupTable( colormapManager.lut ) 
        self.levelSetMapper.UseLookupTableScalarRangeOn()
       
        self.levelSetFilter.SetNumberOfContours( 1 ) 
          
#        levelSetMapper.SetColorModeToMapScalars()  
        self.levelSetActor = vtk.vtkLODActor() 
#            levelSetMapper.ScalarVisibilityOff() 
#            levelSetActor.SetProperty( self.levelSetProperty )              
        self.levelSetActor.SetMapper( self.levelSetMapper )

        self.cursorProperty  = None 
        self.cursor = vtk.vtkSphereSource()
        self.cursor.SetRadius(2.0)
        self.cursor.SetThetaResolution(8)
        self.cursor.SetPhiResolution(8)
        
        mapper = vtk.vtkPolyDataMapper()
        if vtk.VTK_MAJOR_VERSION <= 5:  mapper.SetInput( self.cursor.GetOutput() )
        else:                           mapper.SetInputData( self.cursor.GetOutput() ) 
        self.cursorActor.SetMapper(mapper)        
#        self.createDefaultProperties() 
                                                                       
        self.renderer.AddActor( self.levelSetActor )
        self.surfacePicker.AddPickList( self.levelSetActor )
        self.surfacePicker.PickFromListOn()
        self.renderer.AddViewProp(self.cursorActor)
        self.cursorActor.SetProperty(self.cursorProperty)
        self.levelSetActor.VisibilityOff()
                              
    def buildVolumePipeline(self):
        """ execute() -> None
        Dispatch the vtkRenderer to the actual rendering widget
        """  
        extent = self.input().GetExtent() 
        rangeBounds = self.getRangeBounds() 
        self.sliceCenter = [ (extent[2*i+1]-extent[2*i])/2 for i in range(3) ]       
        spacing = self.input().GetSpacing()
        sx, sy, sz = spacing       
        origin = self.input().GetOrigin()
        ox, oy, oz = origin
        self._range = [ rangeBounds[0], rangeBounds[1], rangeBounds[0], 0 ]
        dataType = self.input().GetScalarTypeAsString()
        self.pos = [ spacing[i]*extent[2*i] for i in range(3) ]
#        if ( (origin[0] + self.pos[0]) < 0.0): self.pos[0] = self.pos[0] + 360.0
        bounds = [0]*6
        for i in range(6):
            io2 = int(i/2)
            bounds[i] = origin[io2] + spacing[io2]*extent[i] 
            
        print " @@@VolumeRenderer@@@   Data Type = %s, range = (%f,%f), max_scalar = %s" % ( dataType, rangeBounds[0], rangeBounds[1], self._max_scalar_value )
        print "Extent: %s " % str( self.input().GetExtent() )
        print "Spacing: %s " % str( spacing )
        print "Origin: %s " % str( origin )
        print "Dimensions: %s " % str( self.input().GetDimensions() )
        print "Bounds: %s " % str( bounds )
        print "Input Bounds: %s " % str( self.input().GetBounds() )
        print "VolumePosition: %s " % str( self.pos )
        
#        self.inputInfo = self.inputPort.GetInformation() 
#        translation = inputInfo.Get( ResampleTranslationKey  )                                     
                        
        # The property describes how the data will look
        self.volumeProperty = vtk.vtkVolumeProperty()
        self.volumeProperty.SetInterpolationType( vtk.VTK_LINEAR_INTERPOLATION )
        self.volumeProperty.SetColor(self.colorTransferFunction)
        self.volumeProperty.SetScalarOpacity(self.opacityTransferFunction)
#        self.volumeProperty.SetGradientOpacity(self.opacityTransferFunction)
     
        # The mapper knows how to render the data

#        self.volumeMapperTexture2D = vtk.vtkVolumeTextureMapper2D()
        self.volumeMapper = vtk.vtkSmartVolumeMapper() 
        self.volumeMapper.SetBlendModeToComposite() 
        self.volumeMapper.CroppingOff()
       
#        self.clipper.AddObserver( 'AnyEvent', self.clipObserver )
        
#        self.volumeMapper.SetScalarModeToUsePointFieldData()

        if vtk.VTK_MAJOR_VERSION <= 5:  self.volumeMapper.SetInput(self.input())
        else:                           self.volumeMapper.SetInputData(self.input())        
           
        # The volume holds the mapper and the property and can be used to
        # position/orient the volume
        self.volume = vtk.vtkVolume()
        self.volume.SetScale( 1.0, 1.0, 1.0 )
        self.volume.SetMapper( self.volumeMapper )     
#        self.volume.SetScale( spacing[0], spacing[1], spacing[2] )   
        self.setVolRenderCfg( None, False )

        self.volume.SetProperty(self.volumeProperty)
#        self.clipper.AddObserver( 'AnyEvent', self.EventWatcher )
               
#        self.input().AddObserver( 'AnyEvent', self.EventWatcher )
        
        self.volume.SetPosition( self.pos )
        self.volume.VisibilityOff()

        self.renderer.AddVolume( self.volume )
        self.renderer.SetBackground( VTK_BACKGROUND_COLOR[0], VTK_BACKGROUND_COLOR[1], VTK_BACKGROUND_COLOR[2] )
        self.setColormap( [ 'jet', 1, 0, 0 ] )

    def buildPipeline(self):

        contour_ispec = self.getInputSpec(  1 )       

        contourInput = contour_ispec.input() if contour_ispec <> None else None
        primaryInput = self.input()
        md = self.getInputSpec().getMetadata()
        self.latLonGrid = md.get( 'latLonGrid', True )

        # Create transfer mapping scalar value to color
        self.colorTransferFunction = vtk.vtkColorTransferFunction()                
        # Create transfer mapping scalar value to opacity
        self.opacityTransferFunction = vtk.vtkPiecewiseFunction()        
        self.setMaxScalarValue( self.input().GetScalarType() )

#        self.contourInput = None if contourModule == None else contourModule.getOutput() 
        # The 3 image plane widgets are used to probe the dataset.    
#        print " Volume Slicer buildPipeline, id = %s " % str( id(self) )
        self.sliceOutput = vtk.vtkImageData()  
        xMin, xMax, yMin, yMax, zMin, zMax = primaryInput.GetExtent()       
        self.slicePosition = [ (xMax-xMin)/2, (yMax-yMin)/2, (zMax-zMin)/2  ]       
        dataType = primaryInput.GetScalarTypeAsString()
        bounds = list(primaryInput.GetBounds()) 
        origin = primaryInput.GetOrigin()
        if (dataType <> 'float') and (dataType <> 'double'):
             self.setMaxScalarValue( primaryInput.GetScalarType() )
#        print "Data Type = %s, range = (%f,%f), extent = %s, origin = %s, bounds=%s, slicePosition=%s" % ( dataType, self.rangeBounds[0], self.rangeBounds[1], str(self.input().GetWholeExtent()), str(origin), str(bounds), str(self.slicePosition)  )
      
        # The shared picker enables us to use 3 planes at one time
        # and gets the picking order right
        lut = self.getLut()
        picker = vtk.vtkCellPicker()
        picker.SetTolerance(0.005) 
                
        if self.planeWidgetZ == None:
            self.planeWidgetZ = ImagePlaneWidget( self, picker, 2 )  
            self.planeWidgetZ.SetRenderer( self.renderer )
#            self.observerTargets.add( self.planeWidgetZ )
            prop3 = self.planeWidgetZ.GetPlaneProperty()
            prop3.SetColor(0, 0, 1)
            self.planeWidgetZ.SetUserControlledLookupTable(1)
            self.planeWidgetZ.SetLookupTable( lut )
       
        self.planeWidgetZ.SetInput( primaryInput, contourInput )
        self.planeWidgetZ.SetPlaneOrientationToZAxes()
        self.planeWidgetZ.PlaceWidget( bounds )
       
        if self.planeWidgetZ.HasThirdDimension(): 
            if (self.planeWidgetX == None): 
                self.planeWidgetX = ImagePlaneWidget( self, picker, 0 )
#               self.observerTargets.add( self.planeWidgetX )
                self.planeWidgetX.SetRenderer( self.renderer )
                prop1 = self.planeWidgetX.GetPlaneProperty()
                prop1.SetColor(1, 0, 0)
                self.planeWidgetX.SetUserControlledLookupTable(1)
                self.planeWidgetX.SetLookupTable( lut )
                
            self.planeWidgetX.SetInput( primaryInput, contourInput )
            self.planeWidgetX.SetPlaneOrientationToXAxes()
            self.planeWidgetX.PlaceWidget( bounds )     
                    
            if self.planeWidgetY == None: 
                self.planeWidgetY = ImagePlaneWidget( self, picker, 1)
                self.planeWidgetY.SetRenderer( self.renderer )
                self.planeWidgetY.SetUserControlledLookupTable(1)
#                self.observerTargets.add( self.planeWidgetY )
                prop2 = self.planeWidgetY.GetPlaneProperty()
                prop2.SetColor(1, 1, 0)
                self.planeWidgetY.SetUserControlledLookupTable(1)
                self.planeWidgetY.SetLookupTable( lut )
            
            self.planeWidgetY.SetInput( primaryInput, contourInput )
            self.planeWidgetY.SetPlaneOrientationToYAxes()       
            self.planeWidgetY.PlaceWidget(  bounds  )     

        self.renderer.SetBackground( VTK_BACKGROUND_COLOR[0], VTK_BACKGROUND_COLOR[1], VTK_BACKGROUND_COLOR[2] )
        self.updateOpacity() 
        self.setColormap( [ 'jet', 1, 0, 0 ] )
        
        if (contour_ispec <> None) and (contour_ispec.input() <> None) and (self.contours == None):
            rangeBounds = self.getRangeBounds(1)
            colormapManager = self.getColormapManager( index=1 )
            self.scaleColormap( rangeBounds, 1 )
#            colormapManager = self.getColormapManager()
            self.generateContours = True   
            self.contours = vtk.vtkContourFilter()
            self.contours.GenerateValues( int(self.NumContours), rangeBounds[0], rangeBounds[1] )
     
            self.contourLineMapperer = vtk.vtkPolyDataMapper()
            self.contourLineMapperer.SetInputConnection( self.contours.GetOutputPort() )
            self.contourLineMapperer.SetScalarRange( rangeBounds[0], rangeBounds[1] )
            self.contourLineMapperer.SetColorModeToMapScalars()
            self.contourLineMapperer.SetLookupTable( colormapManager.lut )
            self.contourLineMapperer.UseLookupTableScalarRangeOn()
        else:
            self.setBasemapCoastlineLineSpecs( [ 1, 1 ] )
            self.setBasemapCountriesLineSpecs( [ 0, 1 ] )
         
        self.modifySlicePlaneVisibility( 0, 'x', False )   
        self.modifySlicePlaneVisibility( 1, 'y', False )   
        self.modifySlicePlaneVisibility( 2, 'z', False )  

      
    def clipObserver( self, caller=None, event=None ):
        print " Clip Observer: %s ", str(event)

    def startClip( self, caller=None, event=None ):
        pass

    def endClip( self, caller=None, event=None ):
        pass
        
    def executeClip( self, caller=None, event=None ):
        np = 6
        self.clipper.GetPlanes( self.clipPlanes )
        if not self.cropRegion: self.cropRegion = [0.0]*np
        for ip in range( np ):
            plane = self.clipPlanes.GetPlane( ip )
            o = plane.GetOrigin()
            self.cropRegion[ip] = o[ ip/2 ]
        self.cropVolume() 
        
    def cropVolume(self, setCropExtent=True ):
        if setCropExtent:
            spacing = self.input().GetSpacing() 
            origin = self.input().GetOrigin()        
            self.cropZextent = [ int( ( self.cropRegion[ip] - origin[ip/2] ) / spacing[ip/2] ) for ip in [4,5] ]
        self.volumeMapper.SetCroppingRegionPlanes( self.cropRegion  ) 
        self.render()

    def rebuildVolume( self ):
        self.volume = vtk.vtkVolume()
        self.volume.SetMapper( self.volumeMapper ) 
        self.volume.SetScale( 1.0, 1.0, 1.0 )   
        self.volume.SetMapper(self.volumeMapper)
        self.volume.SetProperty(self.volumeProperty)        
        self.volume.SetPosition( self.pos )
        self.setVolRenderCfg( None, False )
        self.renderer.AddVolume( self.volume )

    def setActiveScalars( self ):
        pointData = self.input().GetPointData()
        if self.activeLayer:  
            pointData.SetActiveScalars( self.activeLayer )
            print " SetActiveScalars on pointData(%s): %s" % ( str(pointData), self.activeLayer )
        
    def UpdateCamera( self ):
#        self.volume.UseBoundsOff()     
#        print " *** volume visible: %s " % ( self.volume.GetVisibility() )
        aCamera = self.renderer.GetActiveCamera()
        p = aCamera.GetPosition()
        f = aCamera.GetFocalPoint()
#        printArgs( "ResetCameraClippingRange", focal_point=f, cam_pos=p, vol_bounds=bounds )
        self.renderer.ResetCameraClippingRange() 
        
    def toggleVolumeVisibility( self, args, config_function ):
        if self.volume == None:
            self.buildVolumePipeline()
        make_visible = args[1] # .get( 'state', not self.volume.GetVisibility())
        if make_visible: self.volume.VisibilityOn()
        else: self.volume.VisibilityOff()
        self.render()

    def toggleIsosurfaceVisibility( self, args, config_function ):
        if self.levelSetActor == None:
            self.buildIsosurfacePipeline()
        make_visible = args[1] # .get( 'state', not self.levelSetActor.GetVisibility())
        if make_visible:
            self.levelSetActor.VisibilityOn() 
        else: 
            self.levelSetActor.VisibilityOff()
            self.cursorActor.VisibilityOff()
            
        self.render()
        
    def invokeKeyEvent(self, key, ctrl=0 ):
        self.renderWindowInteractor.SetKeyEventInformation( ctrl, 0, key, 1, key )
        self.renderWindowInteractor.InvokeEvent( 'CharEvent' )

#     def toggleSliceVisibility( self ):
#         self.processKeyEvent( 'W', ctrl=1 )
#         self.activeSliceIndex =  ( self.activeSliceIndex + 1 ) % 3
#         for iSlice in range( 3 ):
#             slice = "xyz"[iSlice]
#             if iSlice ==  self.activeSliceIndex: 
#                 plane_index, plane_widget = self.getPlaneWidget( slice )
#                 self.processKeyEvent( slice, None, None, enable=True  )
#         self.render() 
                    
    def EventWatcher( self, caller, event ): 
        print "Event %s on class %s "  % ( event, caller.__class__.__name__ ) 
#        print "  --- Volume Input Extent: %s " % str( self.input().GetWholeExtent() )
        pass          

#     def onKeyEvent(self, eventArgs ):
#         key = eventArgs[0]
#         ctrl = eventArgs[2]
#         if (  ( key == 'W' ) and ctrl        ): 
#             bbar = self.getInteractionButtons()  
#             bbar.releaseSliders() 
#             for index in range(3):  self.modifySlicePlaneVisibility( index, "xyz"[index], False )              
#         else:                   
#             return StructuredGridPlot.onKeyEvent( self, eventArgs )
#         return 1 
        
    def showConfigureButton(self):                                                                                      
        config_button = self.getButton( names=['Configure'] ) # names=['ScaleColormap'] ) Configure
        config_button.On()
        self.render()
               
    def generateCTF( self, ctf_data= None, cmap_index=0, **args  ):
        if ctf_data: self.ctf_data = ctf_data
        else: ctf_data = self.ctf_data
        if ctf_data and (cmap_index==0):
            imageRange = self.getImageValues( ctf_data[0:2], cmap_index ) 
            colormapManager = self.getColormapManager( index=cmap_index )
            colormapManager.setScale( imageRange, ctf_data )
            if len(ctf_data) > 2 :
                self.invert = ctf_data[2]
            self.rebuildColorTransferFunction( imageRange )
#            print " Volume Renderer[%d]: Scale Colormap: ( %.4g, %.4g ) " % ( self.moduleID, ctf_data[0], ctf_data[1] )

    def setColormap( self, data, **args ):
        if StructuredGridPlot.setColormap( self, data, **args ):
            colormapManager = self.getColormapManager( **args ) 
            self.rebuildColorTransferFunction( colormapManager.getImageScale() )
            self.render() 
            
    def printOTF( self ):
        nPts = 20
        tf_range = self.opacityTransferFunction.GetRange()
        dr = ( tf_range[1] - tf_range[0] ) / nPts
        sValues = [ "%.2f" % self.opacityTransferFunction.GetValue( tf_range[0] + iP * dr ) for iP in range( nPts ) ] 
        print "OTF values: ", ' '.join(sValues)
        
    def rebuildColorTransferFunction( self, imageRange ):
        lut = self.getLut()
        self.colorTransferFunction.RemoveAllPoints ()
        nc = lut.GetNumberOfTableValues()
        dr = (imageRange[1] - imageRange[0])             
        for i in range(nc):
            interval_position = float(i)/nc
            data_value = imageRange[0] + dr * interval_position
#                lut_index = (nc-i-1) if self.invert else i
            color = lut.GetTableValue( i )
            self.colorTransferFunction.AddRGBPoint( data_value, color[0], color[1], color[2] )
#                if i % 50 == 0:  print "   --- ctf[%d:%d:%d] --  %.2e: ( %.2f %.2f %.2f ) " % ( i, lut_index, self.invert, data_value, color[0], color[1], color[2] )
        
          
#    def PrintStats(self):
#        print_docs( self.volume.mapper )
#        self.print_traits()
#        print "Volume: bounds=%s, scale=%s, mapper=%s" % ( str(self.volume.bounds), str(self.volume.scale), str(self.volume_mapper_type) )

    def adjustOpacity( self, opacity_data, **args ):
        rangeBounds = self.getRangeBounds()
        maxop = abs( opacity_data[1] ) 
        self.max_opacity = maxop if maxop < 1.0 else 1.0
        range_min, range_max = rangeBounds[0], rangeBounds[1]
#        self.vthresh = opacity_data[0]*(self.seriesScalarRange[1]-self.seriesScalarRange[0])*0.02
        self.updateOTF()
        cmap_index=0
        colormapManager = self.getColormapManager( index=cmap_index )
        self.updatingColormap( cmap_index, colormapManager )
#        printArgs( "adjustOpacity", irange=self._range,  max_opacity=self.max_opacity, opacity_data=opacity_data, vthresh=vthresh, ithresh=self._range[3] )   

    def generateOTF( self, otf_data=None, **args ): 
        if otf_data: self.otf_data = otf_data
        else: otf_data = self.otf_data
        if otf_data:
            if self.transferFunctionConfig:
                self.transferFunctionConfig.setTransferFunctionType( otf_data[2] )
            self._range = self.getImageValues( ( otf_data[0], otf_data[1], 0.0 ) )
            if len( otf_data ) > 3: self.refinement = [ otf_data[3], otf_data[4] ]
            self.updateOTF()
#        printArgs( "generateOTF", irange=self._range,  otf_data=otf_data, refinement=self.refinement  )   
           
    def  getTransferFunctionPoints( self, image_value_range, pointType ):
        zero_point = image_value_range[2] 
        scalar_bounds = [ 0, self._max_scalar_value ]
        points = []
#        print "getTransferFunctionPoints: image_value_range = ( %f %f ), zero_point = %f, refinement = ( %f %f ), max_opacity = %s" % ( image_value_range[0], image_value_range[1], zero_point, self.refinement[0], self.refinement[1], self.max_opacity )             
        if pointType == PositiveValues:
            full_range = [ image_value_range[i] if image_value_range[i] >= zero_point else zero_point for i in range(2) ]
            mid_point = ( full_range[0] + full_range[1] ) / 2.0   
            half_width = ( full_range[1] - full_range[0] ) / 2.0 
            eps = (image_value_range[1]-image_value_range[0]) * .001
            self.getNewNode( points, ( zero_point, 0. ) )
            self.getNewNode( points, ( full_range[0]-eps, 0.0 ), ( zero_point+eps, self.max_opacity), self.refinement[0], color=NodeData.CYAN, index=self.NI_SHAPE_ADJ_0 )
            self.getNewNode( points, ( mid_point-eps, 0.0 ), (mid_point-half_width+eps, self.max_opacity ), self.refinement[1], color=NodeData.CYAN, index=self.NI_SHAPE_ADJ_1 )  
            self.getNewNode( points, ( mid_point, self.max_opacity ), free=True, index=self.NI_RANGE_POSITION ) 
            self.getNewNode( points, ( mid_point + self.refinement[1]*half_width, self.max_opacity * self.refinement[1] ) )            
            self.getNewNode( points, ( zero_point, 0.0 ), ( scalar_bounds[1], 0.0 ), (full_range[1]-zero_point)/(scalar_bounds[1]-zero_point), index=self.NI_RANGE_WIDTH )
            self.getNewNode( points, ( scalar_bounds[1], 0.0 ) )
        elif pointType == NegativeValues:
            eps = (image_value_range[1]-image_value_range[0]) * .001
            data_range = self.getDataValues( image_value_range )
            data_range = [ data_range[i] if data_range[i] >= 0.0 else 0.0 for i in range(2) ]
            full_range = self.getImageValues( [ -data_range[0], -data_range[1] ] )
#            full_range = [ full_range[i] if full_range[i] >= 0.0 else 0.0 for i in range(2) ]
            mid_point = ( full_range[0] + full_range[1] ) / 2.0   
            half_width = ( full_range[0] - full_range[1] ) / 2.0 
            peak_handles = [ mid_point - self.refinement[1]*half_width+eps, mid_point + self.refinement[1]*half_width-eps  ]
            ph_opacity = self.max_opacity * self.refinement[1]
            adjustment_point = full_range[0] + self.refinement[0] * ( zero_point - full_range[0] )
#            if full_range[1] > scalar_bounds[0]: self.getNewNode( points, (scalar_bounds[0], 0. ) )
#            if full_range[1] >= 0: self.getNewNode( points, ( full_range[1], 0.0 ) )
#            elif peak_handles[0] > 0: self.getNewNode( points, ( 0.0, interp_zero( full_range[1], 0.0, peak_handles[0], ph_opacity ) ) )
#            if peak_handles[0] >= 0: self.getNewNode( points, ( peak_handles[0], ph_opacity ) )            
#            elif mid_point > 0: self.getNewNode( points, ( 0.0, interp_zero( peak_handles[0], ph_opacity, mid_point, self.max_opacity ) ) )
#            if mid_point >= 0: self.getNewNode( points, ( mid_point, self.max_opacity ) ) 
#            elif peak_handles[1] > 0: self.getNewNode( points, ( 0.0, interp_zero( mid_point, self.max_opacity, peak_handles[1], ph_opacity ) ) )
#            if peak_handles[1] >= 0: self.getNewNode( points, ( peak_handles[1], ph_opacity ) )  
#            elif adjustment_point > 0: self.getNewNode( points, ( 0.0, interp_zero( peak_handles[1], ph_opacity, adjustment_point, self.refinement[0]*self.max_opacity ) ) )
#            if adjustment_point > 0: self.getNewNode( points, ( adjustment_point, self.refinement[0]*self.max_opacity )  )           
#            else: self.getNewNode( points, ( 0.0, interp_zero( adjustment_point, self.refinement[0]*self.max_opacity, zero_point, 0. ) ) )
            self.getNewNode( points, (scalar_bounds[0], 0. ) )
            self.getNewNode( points, ( full_range[1], 0.0 ) )
            self.getNewNode( points, ( peak_handles[0], ph_opacity ) )            
            self.getNewNode( points, ( mid_point, self.max_opacity ) ) 
            self.getNewNode( points, ( peak_handles[1], ph_opacity ) )  
            self.getNewNode( points, ( adjustment_point, self.refinement[0]*self.max_opacity )  )           
            self.getNewNode( points, ( zero_point, 0. ) )
        elif pointType == AllValues:
            full_range = [ image_value_range[0], image_value_range[1] ]
            mid_point = ( full_range[0] + full_range[1] ) / 2.0   
            half_width = ( full_range[1] - full_range[0] ) / 2.0 
            eps = (image_value_range[1]-image_value_range[0]) * .001
            self.getNewNode( points, (scalar_bounds[0], 0. ) )
            if ( full_range[0] > zero_point ): 
                self.getNewNode( points, ( zero_point, 0. ) )
                self.getNewNode( points, ( full_range[0]-eps, 0.0 ), ( zero_point+eps, self.max_opacity), self.refinement[0], color=NodeData.CYAN, index=self.NI_SHAPE_ADJ_0 )
#                points.append( ( full_range[0] - self.refinement[0] * ( full_range[0] - zero_point ), self.max_opacity * self.refinement[0] ) )
            else: 
                self.getNewNode( points, ( full_range[0], 0.0 ) )
                self.getNewNode( points, ( zero_point, 0.0 ) )
#            points.append( ( mid_point - self.refinement[1]*half_width, self.max_opacity * self.refinement[1] ) ) 
            self.getNewNode( points, ( mid_point-eps, 0.0 ), (mid_point-half_width+eps, self.max_opacity ), self.refinement[1], color=NodeData.CYAN, index=self.NI_SHAPE_ADJ_1 )  
            self.getNewNode( points, ( mid_point, self.max_opacity ), free=True, index=self.NI_RANGE_POSITION ) 
            self.getNewNode( points, ( mid_point + self.refinement[1]*half_width, self.max_opacity * self.refinement[1] ) )            
            if (zero_point > full_range[1] ):  
                self.getNewNode( points, ( full_range[1]+self.refinement[0]*(zero_point-full_range[1]), self.max_opacity*self.refinement[0] ) )
                self.getNewNode( points, ( zero_point, 0. ) )
            else: 
                self.getNewNode( points, ( zero_point, 0.0 ), ( scalar_bounds[1], 0.0 ), (full_range[1]-zero_point)/(scalar_bounds[1]-zero_point), index=self.NI_RANGE_WIDTH )
                self.getNewNode( points, ( scalar_bounds[1], 0.0 ) )
            self.getNewNode( points, ( scalar_bounds[1], 0.) )
        return points
            
    def getNewNode( self, nodeList, rootImagePoint, endImagePoint = None, s=None, **args ):
        n = NodeData( ix0=rootImagePoint[0], y0=rootImagePoint[1], **args )
        n.dx0 = self.getDataValue( rootImagePoint[0] )
        if endImagePoint:
            n.setImageVectorData( endImagePoint, s )
            n.dx1 = self.getDataValue( endImagePoint[0] )
        nodeList.append( n )
        return n
          
    def updateOTF( self  ):
        if self.updatingOTF: return   # Avoid infinite recursion
        self.updatingOTF = True
#        self.setupTransferFunctionConfigDialog()
#        print " Update Volume OTF, self._range = %s, max opacity = %s, refinement = %s  " % ( str( self._range ), str( self.max_opacity ), str( self.refinement ) )
        self.opacityTransferFunction.RemoveAllPoints()  
        transferFunctionType = AbsValueTransferFunction #self.transferFunctionConfig.getTransferFunctionType()
        scalarRange = self.getScalarRange()
#        dthresh = self._range[3]
        if (transferFunctionType == PosValueTransferFunction) or (transferFunctionType == NegValueTransferFunction):
            pointType = PositiveValues if (transferFunctionType == PosValueTransferFunction) else NegativeValues
            nodeDataList = self.getTransferFunctionPoints( self._range, pointType )
            for nodeData in nodeDataList: 
                pos = nodeData.getImagePosition()
                self.opacityTransferFunction.AddPoint( pos[0], pos[1] ) 
#            if self.otf_data: self.transferFunctionConfig.updateGraph( scalarRange, [ 0.0, 1.0 ], nodeDataList )       
        elif transferFunctionType == AbsValueTransferFunction:
            graphData = []
            nodeDataList = self.getTransferFunctionPoints( self._range, NegativeValues )
#            points = []
            pcount = 0
            for nodeData in nodeDataList:  
                pos = nodeData.getImagePosition()
                self.opacityTransferFunction.AddPoint( pos[0], pos[1] ) 
                graphData.append( nodeData  ) 
#                points.append( "\n [%d]--- p(-)[%d]: %s " % ( pcount, nodeData.index, str( nodeData.getDataPosition() ) ) )
                pcount += 1
            nodeDataList = self.getTransferFunctionPoints( self._range, PositiveValues ) 
            for nodeData in nodeDataList:    
                pos = nodeData.getImagePosition()
                self.opacityTransferFunction.AddPoint( pos[0], pos[1] ) 
                graphData.append( nodeData  )  
#                points.append( "\n [%d]--- p(+)[%d]: %s " % ( pcount, nodeData.index, str( nodeData.getDataPosition() ) ) )
                pcount += 1
#            if self.otf_data: self.transferFunctionConfig.updateGraph( scalarRange, [ 0.0, 1.0 ], graphData )
#            print "OTF: [ %s ] " % " ".join( points ) 
        self.updatingOTF = False
#        print "Update OTF: Lighting coefs = %s" % str( [ self.volumeProperty.GetShade(), self.volumeProperty.GetAmbient(), self.volumeProperty.GetDiffuse(), self.volumeProperty.GetSpecular(), self.volumeProperty.GetSpecularPower() ] )
              
    def clearReferrents(self):
        print " **************************************** VolumeSlicer:clearReferrents, id = %d  **************************************** " % self.moduleID
        sys.stdout.flush()
        del self.planeWidgetX
        del self.planeWidgetY
        del self.planeWidgetZ
        self.planeWidgetX = None
        self.planeWidgetY = None
        self.planeWidgetZ = None
        self.latLonGrid = True
        del self.sliceOutput
        self.sliceOutput = None 
        if self.contours:
            del self.contours
            self.contours = None    
            del self.contourLineMapperer 
            self.contourLineMapperer = None
        ispec = self.getInputSpec( 0 ) 
        input0 = ispec.input() 
        print " VolumeSlicer: Input refs = %d " % input0.GetReferenceCount()
        sys.stdout.flush()

        
    def scaleContourColormap(self, data, **args ):
        return self.scaleColormap( data, 1, **args )
        
    def hasContours(self):
        return self.generateContours
        
    def setContourDensity( self, ctf_data, **args ):
        if self.NumContours <> ctf_data[1]:
            self.NumContours = ctf_data[1]
            self.updateContourDensity()

    def getContourDensity( self ):
        return [ 3.0, self.NumContours, 1 ]
    
    def setZScale( self, zscale_data, **args ):
        self.setInputZScale( zscale_data, **args )
        if self.planeWidgetX <> None:
            primaryInput = self.input()
            bounds = list( primaryInput.GetBounds() ) 
            if not self.planeWidgetX.MatchesBounds( bounds ):
                self.planeWidgetX.PlaceWidget( bounds )        
                self.planeWidgetY.PlaceWidget( bounds ) 
        plotButtons = self.fetchPlotButtons()
        cf = plotButtons.getConfigFunction('ZSlider')
        if cf: 
            cf.scaleRange( zscale_data[0] )
        self.render()               

    def setInputZScale( self, zscale_data, **args  ):
        StructuredGridPlot.setInputZScale( self, zscale_data, **args  )
        ispec = self.getInputSpec(  1 )       
        if (ispec <> None) and (ispec.input() <> None):
            contourInput = ispec.input() 
            ix, iy, iz = contourInput.GetSpacing()
            sz = zscale_data[0]
            contourInput.SetSpacing( ix, iy, sz )  
            contourInput.Modified() 
              
    def getOpacity(self):
        return self.opacity
    
    def setOpacity(self, range, **args ):
        self.opacity = range
#        printArgs( " Leveling: ", opacity=self.opacity, range=range ) 
        self.updateOpacity() 

    def updateOpacity(self, cmap_index=0 ):
        colormapManager = self.getColormapManager( index=cmap_index )
        colormapManager.setAlphaRange( [ bound( self.opacity[i], [ 0.0, 1.0 ] ) for i in (0,1) ] )
        if (self.opacityUpdateCount % 5) == 0: self.render()
        self.opacityUpdateCount = self.opacityUpdateCount + 1  
#        self.lut.SetAlpha( self.opacity[1] )
#        self.lut.SetAlphaRange ( self.opacity[0], self.opacity[1] )
#        print "  ---> Set Opacity = %s " % str( self.opacity )
#        self.UpdateWidgetPlacement()
        
#    def UpdateWidgetPlacement(self):
#        self.updatingPlacement = True
#        self.planeWidgetX.UpdatePlacement() 
#        self.planeWidgetX.PlaceWidget()
#        self.planeWidgetY.UpdatePlacement() 
#        self.planeWidgetY.PlaceWidget()
#        self.planeWidgetZ.UpdatePlacement() 
#        self.planeWidgetZ.PlaceWidget()
#        self.updatingPlacement = False

    def enableVisualizationInteraction(self): 
#        print>>sys.stderr, "enable Visualization Interaction"
        if self.planeWidgetX <> None: self.planeWidgetX.EnableInteraction()                                                
        if self.planeWidgetY <> None:self.planeWidgetY.EnableInteraction()                                                
        if self.planeWidgetZ <> None:self.planeWidgetZ.EnableInteraction()  

    def disableVisualizationInteraction(self):
#        print>>sys.stderr, "disable Visualization Interaction" 
        if self.planeWidgetX <> None: self.planeWidgetX.DisableInteraction()                                                
        if self.planeWidgetY <> None:self.planeWidgetY.DisableInteraction()                                                
        if self.planeWidgetZ <> None:self.planeWidgetZ.DisableInteraction()  

    def updatingColormap( self, cmap_index, colormapManager ):
        if cmap_index == 0:
            if self.planeWidgetX <> None: self.planeWidgetX.SetTextureInterpolate( colormapManager.smoothColormap )
            if self.planeWidgetY <> None: self.planeWidgetY.SetTextureInterpolate( colormapManager.smoothColormap )
            if self.planeWidgetZ <> None: self.planeWidgetZ.SetTextureInterpolate( colormapManager.smoothColormap )
            self.updateModule()
            
    def getPlaneWidget( self, plane ):       
        if plane == 'x': return 0, self.planeWidgetX
        if plane == 'y': return 1, self.planeWidgetY
        if plane == 'z': return 2, self.planeWidgetZ
        return None 

#TODO:    
    def modifySlicePlaneVisibility( self, slider_index, plane, make_visible=None ):
        plane_index, plane_widget = self.getPlaneWidget( plane )
        bbar = self.getInteractionButtons()
        if make_visible == None:  
            make_visible = bbar.slidersVisible[ slider_index ] 
        else:
            bbar.slidersVisible[ slider_index ] = make_visible
        if make_visible:   
            plane_widget.VisibilityOn()
            if plane == 'z': 
                if self.generateContours:
                    self.setContourVisibility( slider_index, True ) 
                else: 
                    self.basemapLinesVisibilityOn()
            else:
                self.setContourVisibility( slider_index, True )
        else:               
            plane_widget.VisibilityOff()
            if plane == 'z': 
                if self.generateContours:
                    self.setContourVisibility( slider_index, False ) 
                else: 
                    self.basemapLinesVisibilityOff()
            else:
                self.setContourVisibility( slider_index, False )
                
                                                                        

#        self.set3DOutput() 

        # Add the times series only in regular volume slicer and not in Hovmoller Slicer
#         if self.getInputSpec().getMetadata()['plotType']=='xyz':
#             self.addConfigurableFunction('Show Time Series', None, 't' )

    def buildOutlineMap(self):
        # This function load a binary image (black and white)
        # and create a default grid for it. Then it uses re-gridding algorithms 
        # to scale in the correct domain.
        from pylab import imread
        import vtk.util.vtkImageImportFromArray as vtkUtil

        # read outline image and convert to gray scale
        try:
            data = imread(defaultOutlineMapFile)
            data = data.mean(axis=2)
    
    #        # create a variable using the data loaded in the image and an uniform grid
            dims = data.shape
            reso = [180.0/dims[0], 360.0/dims[1]]
            var = cdms2.createVariable(data)
            lat = cdms2.createUniformLatitudeAxis(90, dims[0], -reso[0])
            lon = cdms2.createUniformLongitudeAxis(-180, dims[1], reso[1])
            var.setAxis(0, lat)
            var.setAxis(1, lon)
    
            # create the final map using the ROI
            ROI = self.roi[:]
            if ROI[2] < -90.0: ROI[2] = -90.0
            if ROI[3] >  90.0: ROI[3] =  90.0
            odims = [ (ROI[3]-ROI[2])/reso[0] , (ROI[1]-ROI[0])/reso[1] ]
            ogrid = cdms2.createUniformGrid( ROI[2], odims[0], reso[0], ROI[0], odims[1], reso[1] )
            ovar = var.regrid(ogrid, regridTool='regrid2')
            
            # replace outlier numbers
            d = ovar.data
            d[d==1e+20] = d[d<>1e+20].max()
            
            img = vtkUtil.vtkImageImportFromArray()
            img.SetArray(ovar.data)
            img.Update()
            
        except Exception:
            print>>sys.stderr, "Error building Outline Map"
            traceback.print_exc()
            return None
        
        # convert to vtkImageData       
        return img.GetOutput()

                
    def updateContourDensity(self):
        if self.generateContours:
            rangeBounds = self.getRangeBounds(1)
            self.contours.GenerateValues( self.NumContours, rangeBounds[0], rangeBounds[1] )
            self.contours.Modified()
            self.render()
                        
    def updateModule(self, **args ):
        primaryInput = self.input()
        contour_ispec = self.getInputSpec(  1 )       
        contourInput = contour_ispec.input() if contour_ispec <> None else None
        if self.planeWidgetX <> None: self.planeWidgetX.SetInput( primaryInput, contourInput )         
        if self.planeWidgetY <> None: self.planeWidgetY.SetInput( primaryInput, contourInput )         
        if self.planeWidgetZ <> None: self.planeWidgetZ.SetInput( primaryInput, contourInput ) 
        if self.baseMapActor: self.baseMapActor.SetVisibility( int( self.enableBasemap ) )
        self.render()
#        self.set3DOutput()
           
    def TestObserver( self, caller=None, event = None ):
        print " Volume Slicer TestObserver: event = %s, " % ( event )
    
    def getLayerColor( self, type ):
        if type == 'coastline':
            return ( 0, 0, 0 )
        if type == 'countries':
            return ( 0.7, 0.2, 0.2 )
        if type == 'states':
            return ( 0.5, 0.5, 0.3 )
        if type == 'lakes':
            return ( 0, 0, 0.6 )
        return ( 0, 0, 0 )
            
    def ProcessIPWAction( self, caller, event, **args ):
        action = args.get( 'action', caller.State )
        iAxis = caller.PlaneIndex

        if event == ImagePlaneWidget.InteractionUpdateEvent:
            
            if action == ImagePlaneWidget.Cursoring:   
                if not self.isSlicing:
                    self.isSlicing = True
                ispec = self.inputSpecs[ 0 ] 
                cursor_data = caller.GetCursorData()
                image_value = cursor_data[3] 
                cpos = cursor_data[0:3]     
                dataValue = ispec.getDataValue( image_value )
                wpos = ispec.getWorldCoords( cpos )
                if self.generateContours:
                    contour_image_value = cursor_data[4]
                    if  contour_image_value:
                        contour_value = self.getDataValue( contour_image_value, 1 )
                        contour_units = self.getUnits(1)
                        textDisplay = " Position: (%s, %s, %s), Value: %.3G %s, Contour Value: %.3G %s" % ( wpos[0], wpos[1], wpos[2], dataValue, ispec.units, contour_value, contour_units )
                    else:
                        textDisplay = " Position: (%s, %s, %s), Value: %.3G %s" % ( wpos[0], wpos[1], wpos[2], dataValue, ispec.units )
#                        print " >>>>> Current Image Value: %d %d, contour value: %.3G, pos = %s " % ( image_value, contour_image_value,  contour_value, str(cpos) ) # , str(wpos) )
                else:
                    textDisplay = " Position: (%s, %s, %s), Value: %.3G %s." % ( wpos[0], wpos[1], wpos[2], dataValue, ispec.units )
#                    print " >>>>> Current Image Value: %d,  pos = %s " % ( image_value, str(cpos) ) # , str(wpos) )
                sliceIndex = caller.GetSliceIndex() 
                self.slicePosition[iAxis] = sliceIndex
                self.updateTextDisplay( textDisplay )
                
                coord = ispec.getWorldCoordsAsFloat(cpos)
                screenPos = caller.GetCurrentScreenPosition()
                self.updateLensDisplay(screenPos, coord)
                
            if action == ImagePlaneWidget.Pushing: 
                ispec = self.inputSpecs[ 0 ] 
                if not self.isSlicing:
                    self.isSlicing = True 
                sliceIndex = caller.GetSliceIndex() 
                axisName, spos = ispec.getWorldCoord( sliceIndex, iAxis, True )
                textDisplay = " %s = %s ." % ( axisName, spos )
                if iAxis == 0:
                    p1 = caller.GetPoint1()
#                    print " >++++++++++++++++++> Slicing: Set Slice[%d], index=%d, pos=%.2f, " % ( iAxis, sliceIndex, p1[0] ), textDisplay
                self.slicePosition[ iAxis ] = sliceIndex                  
                self.updateTextDisplay( textDisplay ) 

                if (iAxis == 2):              
                    origin = caller.GetOrigin()
                    for type in ( 'coastline', 'countries', 'states', 'lakes' ): 
                        line_specs = self.basemapLineSpecs.get( type, None )
                        polys_list = self.shapefilePolylineActors.get( type, None )
                        density = int( round( line_specs[1] ) ) if line_specs else 1
                        polys = polys_list[ density ] if polys_list else None
                        if polys:
                            pos = polys.GetPosition()
                            pos1 = [ pos[0], pos[1], origin[2] ]
                            polys.SetPosition( pos1 )
            
                    if self.generateContours:
                        slice_data = caller.GetReslice2Output()
                        if slice_data:
                            iextent =  slice_data.GetExtent()            
                            ispacing =  slice_data.GetSpacing() 
                            if vtk.VTK_MAJOR_VERSION <= 5:  self.contours.SetInput( slice_data )
                            else:                           self.contours.SetInputData( slice_data )                                    
                            self.contours.Modified()
                            origin = caller.GetOrigin()
                            contourLineActor = self.getContourActor( iAxis )
                            contourLineActor.SetPosition( origin[0], origin[1], origin[2] )
            #                contourLineActor.SetOrigin( origin[0], origin[1], origin[2] )
                            self.setVisibleContour( iAxis )
#                print " Generate Contours, data dims = %s, origin = %s, pos = %s, extent = %s" % ( str( slice_data.GetDimensions() ), str(slice_data.GetOrigin()), str(origin), str(slice_data.GetExtent()) )
                
            self.render()
#                print " Generate Contours, data dims = %s, pos = %s %s %s " % ( str( slice_data.GetDimensions() ), str(pos1), str(pos2), str(origin) )

    def setContourActorOrientation( self, iAxis, contourLineActor ):
        if iAxis == 1: 
            contourLineActor.SetOrientation(90,0,0)
        elif iAxis == 0: 
            contourLineActor.SetOrientation(90,0,90)   

    def updateContourActorOrientations( self ):
        for contourLineActorItem in self.contourLineActors.items():
            if contourLineActorItem[1].GetVisibility( ): 
                self.setContourActorOrientation( contourLineActorItem[0], contourLineActorItem[1] )
        self.render()
        pass

                                     
    def getContourActor( self, iAxis, **args ):
        contourLineActor = self.contourLineActors.get( iAxis, None )
        if contourLineActor == None:
            contourLineActor = vtk.vtkActor()
            contourLineActor.SetMapper(self.contourLineMapperer)
            contourLineActor.GetProperty().SetLineWidth(2) 
            contourLineActor.VisibilityOff( )    
            self.renderer.AddActor( contourLineActor ) 
            self.contourLineActors[iAxis] = contourLineActor
            self.setContourActorOrientation( iAxis, contourLineActor )
#            print " GetContourActor %d, origin = %s, position = %s " % ( iAxis, str( contourLineActor.GetOrigin() ), str( contourLineActor.GetPosition() ) )
        return contourLineActor

            
    def setVisibleContour( self, iAxis ):
        for contourLineActorItem in self.contourLineActors.items():
            if iAxis == contourLineActorItem[0]:    contourLineActorItem[1].VisibilityOn( )
            else:                                   contourLineActorItem[1].VisibilityOff( )

    def setContourVisibility( self, iAxis, isVisible ):
        contourLineActor = self.contourLineActors.get( iAxis, None )
        if contourLineActor <> None:
            if isVisible:    contourLineActor.VisibilityOn( )
            else:            contourLineActor.VisibilityOff( )
       
    def getAdjustedSliceExtent( self ):
        ext = None
        if self.iOrientation == 1:      ext = [ 0, self.sliceOutputShape[1]-1,  0, self.sliceOutputShape[0]-1, 0, 0 ]  
        else:                           ext = [ 0, self.sliceOutputShape[0]-1,  0, self.sliceOutputShape[1]-1, 0, 0 ]  
#        print " Slice Extent = %s " % str( ext )
        return ext       

    def getAdjustedSliceSpacing( self, outputData ):
        padded_extent = outputData.GetWholeExtent()
        padded_shape = [ padded_extent[1]-padded_extent[0]+1, padded_extent[3]-padded_extent[2]+1, 1 ]
        padded_spacing = outputData.GetSpacing()
        scale_factor = [ padded_shape[0]/float(self.sliceOutputShape[0]), padded_shape[1]/float(self.sliceOutputShape[1]) ]
        if self.iOrientation == 1:      spacing = [ padded_spacing[1]*scale_factor[1], padded_spacing[0]*scale_factor[0], 1.0 ]
        else:                           spacing = [ padded_spacing[0]*scale_factor[0], padded_spacing[1]*scale_factor[1], 1.0 ]
#        print " Slice Spacing = %s " % str( spacing )
        return spacing
                       
    def initColorScale( self, caller, event ): 
        x, y = caller.GetEventPosition()
        self.ColorLeveler.startWindowLevel( x, y )

    def scaleColormap( self, ctf_data, cmap_index=0, **args ):
        ispec = self.inputSpecs.get( cmap_index, None )
        if ispec and ispec.input(): 
            colormapManager = self.getColormapManager( index=cmap_index )
#            if not colormapManager.matchDisplayRange( ctf_data ):
            imageRange = self.getImageValues( ctf_data[0:2], cmap_index ) 
            colormapManager.setScale( imageRange, ctf_data )
            if self.contourLineMapperer: 
                self.contourLineMapperer.Modified()
            self.updatingColormap( cmap_index, colormapManager )
            ispec.addMetadata( { 'colormap' : self.getColormapSpec(), 'orientation' : self.iOrientation } )
#            print '-'*50
#            print " Volume Slicer[%d]: Scale Colormap: ( %.4g, %.4g ) " % ( self.moduleID, ctf_data[0], ctf_data[1] )
#            print '-'*50
                

    def updateColorScale( self, caller, event ):
        x, y = caller.GetEventPosition()
        wsize = self.renderer.GetSize()
        range = self.ColorLeveler.windowLevel( x, y, wsize )
        return range
                  

