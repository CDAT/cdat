'''
Created on Apr 29, 2014

@author: tpmaxwel
'''
    
import sys, vtk, cdms2, traceback, os, cdtime, math 
from ColorMapManager import *  
from Shapefile import shapeFileReader     
from DistributedPointCollections import kill_all_zombies
from StructuredGridPlot import  *
#from ConfigFunctions import *
import numpy as np

LegacyAbsValueTransferFunction = 0
LinearTransferFunction = 1 
PosValueTransferFunction = 2  
NegValueTransferFunction = 3  
AbsValueTransferFunction = 4

PositiveValues = 0
NegativeValues = 1
AllValues = 2
    
def distance( p0, p1 ):
    dp = [ (p0[i]-p1[i]) for i in range(3) ]
    return math.sqrt( dp[0]*dp[0] + dp[1]*dp[1] + dp[2]*dp[2] )

def interp_zero( x0, y0, x1, y1 ):
    return y0 + (y1-y0)*(-x0/(x1-x0))

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
    
 
class VolumePlot(StructuredGridPlot): 

    NI_RANGE_POSITION = 10001
    NI_RANGE_WIDTH = 10002
    NI_SHAPE_ADJ_0 = 10003
    NI_SHAPE_ADJ_1 = 10004
    
    def __init__( self,  **args ):
        StructuredGridPlot.__init__( self,  **args  )
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
#        self.setupTransferFunctionConfigDialog()
        self.addConfigurableSliderFunction( 'colorScale', 'C', label='Colormap Scale', interactionHandler=self.processColorScaleCommand )
#       self.addConfigurableLevelingFunction( 'colorScale',    'C', label='Colormap Scale', units='data', setLevel=self.generateCTF, getLevel=self.getSgnRangeBounds, layerDependent=True, adjustRangeInput=0, group=ConfigGroup.Color )
        self.addConfigurableSliderFunction( 'functionScale', 'T', label='Transfer Function Range', interactionHandler=self.processThresholdRangeCommand )
#        self.addConfigurableLevelingFunction( 'functionScale', 'T', label='Transfer Function Scale', units='data', setLevel=self.generateOTF, getLevel=self.getAbsRangeBounds, layerDependent=True, adjustRangeInput=0, initRefinement=[ self.refinement[0], self.refinement[1] ], gui=self.transferFunctionConfig, group=ConfigGroup.Rendering  )
        self.addConfigurableSliderFunction( 'opacityScale', 'o', label='Opacity Scale', range_bounds=[ 0.0, 1.0 ], initValue=[ 1.0, 1.0 ], interactionHandler=self.processOpacityScalingCommand )
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
            self.adjustOpacity( config_function.initial_value )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            opacityRange.setValue( args[1], args[2] )
            orange = opacityRange.getValues()
            self.setOpacity( orange )

    def processColorScaleCommand( self, args, config_function = None ):
        colorScaleRange = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            init_range = self.getSgnRangeBounds()
            config_function.range_bounds = init_range  
            config_function.initial_value = init_range  
            self.generateCTF( init_range )
            colorScaleRange.setValues( init_range )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            colorScaleRange.setValue( args[1], args[2] )
            cscale = colorScaleRange.getValues()
            self.generateCTF( cscale )

    def processThresholdRangeCommand( self, args, config_function = None ):
        volumeThresholdRange = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            init_range = self.getSgnRangeBounds()
            config_function.range_bounds = init_range  
            config_function.initial_value = init_range  
            self.generateOTF( init_range )
            volumeThresholdRange.setValues( init_range )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            vt_range = self.getSgnRangeBounds()
            vt_range[ args[1] ] = args[2]
            self.generateOTF( vt_range )
            volumeThresholdRange.setValues( vt_range )
 
    def resetCamera(self):
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
            if self.volumeMapper.GetCropping():
                self.cropVolume( False ) 
#         self.volume.Modified()
#         self.updateModule()
         
    def activateEvent( self, caller, event ):
        StructuredGridPlot.activateEvent( self, caller, event )
        if self.clipper and ( self.cropRegion == None ):
            self.renwin = self.renderer.GetRenderWindow( )
            if self.renwin <> None:
                iren = self.renwin.GetInteractor() 
                if ( iren <> None ): 
                    self.clipper.SetInteractor( iren )
                    self.cropRegion = self.getVolumeBounds()
                    self.clipper.PlaceWidget( self.cropRegion )

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
        self.persistCropRegion()
                   
    def toggleClipping( self, enableClipping ):
        self.clipping_enabled = enableClipping 
        self.volumeMapper.CroppingOn()
        if self.clipping_enabled and self.isInSelectedCell:     self.clipOn()
        else:                                                   self.clipOff()
    
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

#TODO:
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

                              
    def buildPipeline(self):
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
        self.setMaxScalarValue( self.input().GetScalarType() )
        self.pos = [ spacing[i]*extent[2*i] for i in range(3) ]
#        if ( (origin[0] + self.pos[0]) < 0.0): self.pos[0] = self.pos[0] + 360.0
        bounds = [0]*6
        for i in range(6):
            io2 = int(i/2)
            bounds[i] = origin[io2] + spacing[io2]*extent[i] 
            
        print " @@@VolumeRenderer@@@   Data Type = %s, range = (%f,%f), max_scalar = %s" % ( dataType, rangeBounds[0], rangeBounds[1], self._max_scalar_value )
        print "Extent: %s " % str( self.input().GetWholeExtent() )
        print "Spacing: %s " % str( spacing )
        print "Origin: %s " % str( origin )
        print "Dimensions: %s " % str( self.input().GetDimensions() )
        print "Bounds: %s " % str( bounds )
        print "Input Bounds: %s " % str( self.input().GetBounds() )
        print "VolumePosition: %s " % str( self.pos )
        
#        self.inputInfo = self.inputPort.GetInformation() 
#        translation = inputInfo.Get( ResampleTranslationKey  )                                     
        
        # Create transfer mapping scalar value to color
        self.colorTransferFunction = vtk.vtkColorTransferFunction()
                
        # Create transfer mapping scalar value to opacity
        self.opacityTransferFunction = vtk.vtkPiecewiseFunction()        
                
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
       
        self.clipper = vtk.vtkBoxWidget()
        self.clipper.RotationEnabledOff()
        self.clipper.SetPlaceFactor( 1.0 )    
        self.clipper.AddObserver( 'StartInteractionEvent', self.startClip )
        self.clipper.AddObserver( 'EndInteractionEvent', self.endClip )
        self.clipper.AddObserver( 'InteractionEvent', self.executeClip )
#        self.clipper.AddObserver( 'AnyEvent', self.clipObserver )
        
#        self.volumeMapper.SetScalarModeToUsePointFieldData()

#         if vtk.VTK_MAJOR_VERSION <= 5:  self.volumeMapper.SetInput(self.input())
#         else:                           self.volumeMapper.SetInputData(self.input())        
           
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

        self.renderer.AddVolume( self.volume )
        self.renderer.SetBackground( VTK_BACKGROUND_COLOR[0], VTK_BACKGROUND_COLOR[1], VTK_BACKGROUND_COLOR[2] )
        self.setColormap( [ 'jet', 1, 0, 0 ] )

      
    def clipObserver( self, caller=None, event=None ):
        print " Clip Observer: %s ", str(event)

    def startClip( self, caller=None, event=None ):
        self.clearCellSelection()

    def endClip( self, caller=None, event=None ):
        pass
        
    def executeClip( self, caller=None, event=None ):
        planes = vtk.vtkPlanes(); np = 6
        self.clipper.GetPlanes(planes)
        if not self.cropRegion: self.cropRegion = [0.0]*np
        for ip in range( np ):
            plane = planes.GetPlane( ip )
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
            print " SetActiveScalars on pointData(%s): %s" % ( addr(pointData), self.activeLayer )

    def UpdateCamera( self ):
#        self.volume.UseBoundsOff()     
#        print " *** volume visible: %s " % ( self.volume.GetVisibility() )
        aCamera = self.renderer.GetActiveCamera()
        bounds = self.volume.GetBounds()
        p = aCamera.GetPosition()
        f = aCamera.GetFocalPoint()
#        printArgs( "ResetCameraClippingRange", focal_point=f, cam_pos=p, vol_bounds=bounds )
        self.renderer.ResetCameraClippingRange() 
        
    def EventWatcher( self, caller, event ): 
        print "Event %s on class %s "  % ( event, caller.__class__.__name__ ) 
#        print "  --- Volume Input Extent: %s " % str( self.input().GetWholeExtent() )
        pass          
                                                                                               
    def onKeyPress( self, caller, event ):
        key = caller.GetKeyCode() 
        keysym = caller.GetKeySym()
#        print " -- Key Press: %c ( %d: %s ), event = %s " % ( key, ord(key), str(keysym), str( event ) )
               
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
        
 
