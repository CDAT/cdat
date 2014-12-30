'''
Created on Apr 30, 2014

@author: tpmaxwel
'''
from ColorMapManager import *
from ButtonBarWidget import *
import vtk, traceback, time, threading
MIN_LINE_LEN = 150
VTK_NOTATION_SIZE = 10

PlotButtonNames = [ 'XSlider', 'YSlider', 'ZSlider', 'ToggleSurfacePlot', 'ToggleVolumePlot' ]

def addState( values, state ):
    state_val = 'vcs.on' if ( state == 1 ) else 'vcs.off'
    for index, val in enumerate(values):
        if val in [ "vcs.on", "vcs.off" ]:
            values[ index ] = state_val
            return
    values.append( state_val )

class AnimationStepper:
    
    def __init__( self, target ):
        self.target = target
    
    def startAnimation(self):
        self.target.startAnimation()

    def stopAnimation(self):
        self.target.stopAnimation()

def ffmpeg( movie, rootFileName ):
    cmd = 'ffmpeg -y -b:v 1024k '
#    bitrate = 1024
#    rate = 10
#    options=''
    cmd += '-i %s%%d.png' % ( rootFileName )
#    cmd += ' -r %s ' % rate
 #   cmd += ' -b:v %sk' % bitrate
 #   cmd += ' ' + options
    cmd += ' ' + movie
    print "Exec: ", cmd
    o = os.popen(cmd).read()
    return o

def saveAnimation( saveDir, animation_frames ):
    writer = vtk.vtkFFMPEGWriter()
    movie = os.path.join( saveDir, "movie.avi" )
    writer.SetFileName( movie )
    print "Saving recorded animation to %s" % movie; sys.stdout.flush()
    writer.SetBitRate(1024*1024*30)
    writer.SetBitRateTolerance(1024*1024*3)
    writer.SetInputData( animation_frames[0] )
    writer.Start()
    for index, frame in enumerate( animation_frames ):
        writer.SetInputData(frame)
        writer.Write()
        time.sleep(0.0)          
    writer.End() 
        

def saveAnimationFFMpeg( animation_frames, saveDir ):
    rootFileName = os.path.join( saveDir, "frame-" )
    files = []
    print " Saving animation (%d frames) to '%s'" % ( len( animation_frames ) , saveDir ); sys.stdout.flush()
    for index, frame in enumerate( animation_frames ):        
        writer = vtk.vtkPNGWriter()
        writer.SetInputData(frame)
        fname = "%s%d.png" % ( rootFileName, index )
        writer.SetFileName( fname )
        writer.Update()
        writer.Write()
        files.append( fname )
        time.sleep( 0.0 )
    ffmpeg( os.path.join( saveDir, "movie.avi" ), rootFileName )
    for f in files: os.remove(f)
    print "Done saving animation"; sys.stdout.flush()

class SaveAnimation():
    
    def __init__( self, frames, **args ):
        self.animation_frames = list( frames )
        self.rootDirName = args.get( 'root_name', "~/.uvcdat/animation"  )
        print "Init SaveAnimationThread, nframes = %d" % len( self.animation_frames )    
        
    def getUnusedDirName( self ):
        for index in range( 100000 ):
            dir_name = os.path.expanduser( '-'.join( [ self.rootDirName, str(index) ]) )
            if not os.path.exists(dir_name):
                os.mkdir( dir_name, 0755  )
                return dir_name
         
    def run(self): 
        nframes =  len( self.animation_frames )              
        if nframes > 0:           
            saveDir = self.getUnusedDirName()
            t = threading.Thread(target=saveAnimation, args = ( saveDir, self.animation_frames ))
            t.daemon = True
            t.start() 
            self.animation_frames = []
                        
class TextDisplayMgr:
    
    def __init__( self, renderer ):
        self.renderer = renderer
    
#     def setTextPosition(self, textActor, pos, size=[400,20] ):
#  #       vp = self.renderer.GetSize()
# #        vpos = [ pos[i]*vp[i] for i in [0,1] ]
# #        textActor.GetPositionCoordinate().SetValue( vpos[0], vpos[1] ) 
#         textActor.SetPosition( 0.2, 0.5 )
#         textActor.SetWidth( 0.6 ) 
#         textActor.SetHeight( 0.08 )     
# #        textActor.GetPosition2Coordinate().SetValue( vpos[0] + size[0], vpos[1] + size[1] )      
  
    def getTextActor( self, aid, text, **args ):
        if text == None: return
        textActor = self.getProp( 'vtkTextActor', aid  )
        if textActor == None:
            textActor = self.createTextActor( aid, **args  )
            self.renderer.AddViewProp( textActor )
        textActor.GetPositionCoordinate().SetCoordinateSystemToNormalizedViewport ()
        textActor.GetPosition2Coordinate().SetCoordinateSystemToNormalizedViewport ()
        textActor.GetPositionCoordinate().SetValue( .3, .9, 0 )
        textActor.GetPosition2Coordinate().SetValue( .98, .98, 0 )
#        textActor.SetWidth( 0.6 ) 
#        textActor.SetHeight( 0.08 )     
        text_lines = text.split('\n')
        linelen = len(text_lines[0])
        if linelen < MIN_LINE_LEN: text += (' '*(MIN_LINE_LEN-linelen)) 
        text += '.' 
        textActor.SetInput( text )
        textActor.Modified()
        return textActor

    def getProp( self, ptype, pid = None ):
        try:
            props = self.renderer.GetViewProps()
            nitems = props.GetNumberOfItems()
            for iP in range(nitems):
                prop = props.GetItemAsObject(iP)
                if prop.IsA( ptype ):
                    if not pid or (prop.id == pid):
                        return prop
        except: 
            pass
        return None
  
    def createTextActor( self, aid, **args ):
        textActor = vtk.vtkTextActor()  
#        textActor.SetTextScaleModeToViewport()
#        textActor.SetTextScaleMode( vtk.vtkTextActor.TEXT_SCALE_MODE_PROP )  
#        textActor.SetMaximumLineHeight( 0.005 ) 
#        print dir( textActor ) 
#        textActor.ScaledTextOn()  
        textActor.SetTextScaleModeToProp()   
        textprop = textActor.GetTextProperty()
        textprop.SetColor( *args.get( 'color', ( VTK_FOREGROUND_COLOR[0], VTK_FOREGROUND_COLOR[1], VTK_FOREGROUND_COLOR[2] ) ) )
        textprop.SetOpacity ( args.get( 'opacity', 1.0 ) )
        textprop.SetFontSize( 8 )
        if args.get( 'bold', False ): textprop.BoldOn()
        else: textprop.BoldOff()
        textprop.ItalicOff()
        textprop.ShadowOff()
        textprop.SetJustificationToLeft()
        textprop.SetVerticalJustificationToBottom()        
        textActor.VisibilityOff()
        textActor.id = aid
        return textActor 
        
class DV3DPlot():  
    
    NoModifier = 0
    ShiftModifier = 1
    CtrlModifier = 2
    AltModifier = 3
    
    LEFT_BUTTON = 0
    RIGHT_BUTTON = 1

    sliceAxes = [ 'x', 'y', 'z' ]       

    AnimationTimerType = 9
    AnimationEventId = 9
    AnimationExternalEventId = 10 
 
    def __init__( self,  **args ):
        self.ParameterValueChanged = SIGNAL( 'ParameterValueChanged' )
        self.type = args.get( 'gmname', 'default').lower()
        self.activate_display=args.get('display',True)
        self.renderer = None
        self.useDepthPeeling = False
        self.record_animation = False
        self.animation_frames = []
        self.renderWindowInteractor = None
        self.inputSpecs = {}
        self.logoActor = None
        self.logoVisible = True
        self.logoRepresentation = None 
        self.setAnimationStepper( AnimationStepper )
        self.labelBuff = ""
        self.resizingWindow = False
        self.textDisplayMgr = None
        if self.activate_display:
            self.createRenderWindow( **args ) 
        self.cameraOrientation = {}
        self.maxStageHeight = 100.0
        self.observerTargets = set()
        self.currTime = None
        self.xcenter = 100.0
        self.xwidth = 300.0
        self.ycenter = 0.0
        self.ywidth = 180.0
        self.cfgManager = ConfigManager( **args )           
        self.buttonBarHandler = ButtonBarHandler( self.cfgManager, **args ) 
        self.plot_attributes = args.get( 'plot_attributes', {} )
        self.plotConstituents = { 'Slice' : 'SliceRoundRobin', 'Volume' : 'ToggleVolumePlot', 'Surface' : 'ToggleSurfacePlot' }  
        self.topo = PlotType.Planar
        
        self.configuring = False
        self.animating = False
        self.activated = False
#        self.buttons = {}
        self.renderWindowSize = None
        self.renderWindowInitSize = None
        self.animationTimerId = -1 

        self.isAltMode = False
        self.createColormap = True
        self.colormapManagers= {}
        self.colormapWidget = None 
        self.colormapWindowSize = None
        self.keyPressHandlers = {}
        interactionButtons = self.getInteractionButtons()
        interactionButtons.addSliderButton( names=['BasemapOpacity'], key='B', toggle=True, label='Basemap Opacity', sliderLabels='Basemap Opacity', interactionHandler=self.processBasemapOpacityCommand, range_bounds=[ 0.0, 1.0 ], initValue= 0.5 )
        interactionButtons.addSliderButton( names=['VerticalScaling'], key='Z', toggle=True, label='Vertical Scaling', sliderLabels='Vertical Scale', interactionHandler=self.processVerticalScalingCommand, range_bounds=[ 0.02, 20.0 ], initValue= 1.0 )
        interactionButtons.addConfigButton( names=['ChooseColormap'], key='m', toggle=True, interactionHandler=self.processChooseColormapCommand, initValue=[ 'jet', False, False ]  )
        interactionButtons.addConfigButton( names=['ToggleClipping'], key='X', toggle=True, parents=['ToggleVolumePlot', 'ToggleSurfacePlot'], interactionHandler=self.processToggleClippingCommand  )
        interactionButtons.addConfigButton( names=['Colorbar'], key='b', toggle=True, label='Show Colorbar', interactionHandler=self.processShowColorbarCommand )
        interactionButtons.addSliderButton( names=['Animation'], key='a', toggle=True, label='Animation', sliderLabels='Speed (Step Delay)', interactionHandler=self.processAnimationCommand, range_bounds=[ 0.0, 2.0 ], initValue=0.0  )
        cameraFunction = self.cfgManager.getConfigurableFunction( 'Camera', interactionHandler=self.processCameraCommand )
        self.addKeyPressHandler( 'r', self.resetCamera )
        self.addKeyPressHandler( 'q',  self.quit )
        self.addKeyPressHandler( 'Q',  self.quit )
        self.addKeyPressHandler( 's',  self.saveState )
        self.addKeyPressHandler( 'p',  self.printParameterValues )
        
    def setAnimationStepper( self, stepper_class ):
        self.animationStepper = stepper_class(self)
        
    def applyAction( self, action ):
        print "Applying action: ", str(action)

    def getControlBar(self, config_function, build_args, **args ):
        control_bar = self.buttonBarHandler.createControlBar( config_function.cfg_state, self.renderWindowInteractor, build_args, position = ( 0.55, 0.08 ), **args )
        control_bar.reposition()
        return control_bar

    def getConstituentSelectionBar(self, config_function, build_args, **args ): 
        args[ 'toggle' ] = True
        control_bar = self.buttonBarHandler.createControlBar( config_function.cfg_state, self.renderWindowInteractor, build_args, position = ( 0.7, 0.07 ), **args )
        control_bar.reposition()
        activations = [ ( cname, True, 1 if self.isConstituentConfigEnabled(cname) else 0 ) for cname in build_args[0] ]
        control_bar.changeButtonActivations( activations )
        return control_bar

    def getConstituentSelectionButton(self, config_function, build_args, position, **args ):    
        control_bar = self.buttonBarHandler.createControlBar( config_function.cfg_state, self.renderWindowInteractor, build_args, position = position, **args )
        control_bar.reposition()
        return control_bar
    
    def processConfigParameterChange( self, parameter, val_key = None ):
#        values = parameter.getValue(val_key)
#        if values == None: values = parameter.getValues()
#        if not hasattr( values, '__iter__' ): values = [ values ]
#        state = parameter.getState()
#        if state <> None: addState( values, state )
        values = parameter.values
        active_constituents = self.getActiveConstituentNames()
        argList = [ parameter.name, parameter.ptype, str(values), active_constituents ]
        self.ParameterValueChanged( argList )

    def getActiveConstituentNames(self):
        active_constituents = []
        for cname in self.plotConstituents.keys():
            if self.isConstituentConfigEnabled(cname):
	            active_constituents.append( cname )
        return active_constituents

    def processConfigStateChange( self, parameter ):
        argList = [ parameter.name, parameter.ptype, str( parameter.getValue('state') ) ] 
        self.ParameterValueChanged( argList )
        
    def addKeyPressHandler( self, key, handler ):
        handlers = self.keyPressHandlers.setdefault( key, [] )
        handlers.append( handler )
        
    def refresh(self):
        self.onWindowModified()
        
    def onClosing(self):
        print "Closing!"
        self.stopAnimation()
        self.terminate()
        
#         pipeline = DV3DPipelineHelper.getPipeline( cell_address, sheetName )
#         if pipeline == None: pipeline = self.getCurrentPipeline()
#         if pipeline: UVCDATGuiConfigFunction.clearModules( pipeline )
            
#        IVModuleConfigurationDialog.reset()
#        StandardGrid.clear_cache()
#        self.cellWidget = None 
#        self.builtCellWidget = False    
 
        self.renderer.RemoveAllViewProps()
        self.clearReferrents()
        if self.renderWindowInteractor <> None:
            self.renderWindowInteractor.TerminateApp()

    def terminate( self ):
        pass
    
    def quit( self, **args ):
        eventArgs = args.get( 'args', None )
        if eventArgs and ( eventArgs[1] == 'Q' ):
            self.saveState()
        self.onClosing()
        sys.exit( 0 )

    def stepAnimation(self, **args): 
        if self.record_animation: self.captureFrame()
   
    def stepAnimationSignal(self):
        self.renderWindowInteractor.SetTimerEventId( self.AnimationExternalEventId )
        self.renderWindowInteractor.SetTimerEventType( self.AnimationTimerType )
        self.renderWindowInteractor.InvokeEvent('TimerEvent')

    def processTimerEvent(self, caller, event):
        eid = caller.GetTimerEventId ()
        etype = caller.GetTimerEventType()
#        print "processTimerEvent: %d %d " % ( eid, etype )
        if self.animating and ( etype == self.AnimationTimerType ):
            self.runAnimation()
        return 1
    
    def getAnimationDelay(self):
        plotButtons = self.getInteractionButtons()
        cf = plotButtons.getConfigFunction('Animation')
        event_duration = 0
        if cf <> None:
            animation_delay = cf.value.getValues()
            event_duration = event_duration + int( animation_delay[0]*1000 )
        return event_duration
            
    def runAnimation( self ):        
        self.stepAnimation( )
        self.updateTimer()

    def updateTimer( self ):
        event_duration = self.getAnimationDelay()
        if self.animationTimerId <> -1: 
            self.renderWindowInteractor.DestroyTimer( self.animationTimerId  )
            self.animationTimerId = -1
        self.renderWindowInteractor.SetTimerEventId( self.AnimationEventId )
        self.renderWindowInteractor.SetTimerEventType( self.AnimationTimerType )
        self.animationTimerId = self.renderWindowInteractor.CreateOneShotTimer( event_duration )
        
    def captureFrame( self, **args ):   
        frameCaptureFilter = vtk.vtkWindowToImageFilter()
        frameCaptureFilter.SetInput( self.renderWindow )
        ignore_alpha = args.get( 'ignore_alpha', True )
        if ignore_alpha:    frameCaptureFilter.SetInputBufferTypeToRGB()
        else:               frameCaptureFilter.SetInputBufferTypeToRGBA()
        frameCaptureFilter.Update()
        output = frameCaptureFilter.GetOutput()
        self.animation_frames.append( output )
                
    def saveAnimation(self):
        saveAnimationThread = SaveAnimation( self.animation_frames )
        self.animation_frames =[]
        saveAnimationThread.run()
        
    def changeButtonActivation(self, button_name, activate, state = None ):
        button = self.buttonBarHandler.findButton( button_name ) 
        print " ---> change Button Activation[%s], activate = %s, state = %s" % ( button_name, str(activate), str(state) )
        if button: 
            if activate: button.activate()
            else: button.deactivate()
        if state <> None:
            button.setToggleState( state )
            
    def changeButtonActivations(self, activation_list ):
        print " ** Change Button Activations: ", str( activation_list )
        for activation_spec in activation_list:
            self.changeButtonActivation( *activation_spec )
                        
    def saveState(self, **args): 
        self.recordCamera()
        self.cfgManager.saveState()

    def getStateData(self, **args): 
        return self.cfgManager.getStateData()

    def getConfigurationData(self, **args): 
        return self.cfgManager.getConfigurationData( **args )

    def getConfigurationParms(self, **args): 
        return self.cfgManager.getConfigurationParms( **args )

    def printParameterValues( self, **args ):
        self.recordCamera()
        parameter_names = list( self.cfgManager.getParameterList() ) + PlotButtonNames
        for param_name in parameter_names:
            print '%s = %s' % ( param_name, self.cfgManager.getParameterValue( param_name ) )
            
    def processKeyPressHandler( self, key, eventArgs ):
#        print " processKeyPress: ", str( key )
        handlers = self.keyPressHandlers.get( key, [] )
        for handler in handlers: handler( args=eventArgs )
        return len( handlers )

    def processBasemapOpacityCommand( self, args, config_function ):
        pass 
            
    def processVerticalScalingCommand( self, args, config_function ):
        pass 

    def processCameraCommand( self, args, config_function ):
        if args and args[0] == "Init":
#            cameraSpecs = config_function.value
            self.initCamera()
#            print " processCameraCommand: "
    
    def processToggleClippingCommand( self, args, config_function ):
        pass 
     
    def getRenderer(self):
        if self.renderer <> None: return self.renderer
        return self.renderWindow.GetRenderers().GetFirstRenderer ()

    def processShowColorbarCommand( self, args, config_function = None ):
        if args and args[0] == "InitConfig":
            constituent = 'Slice'
            state = args[1]
            cs_button = self.getConstituentSelectionButton( config_function, [ ( self.plotConstituents.keys(), ),  self.processColorbarConstituentSelection ], ( 0.9, 0.2) )
            if state: cs_button.show()
            else:     cs_button.hide()
            colorbarParameter = self.cfgManager.getParameter( 'Colorbar' )
            constituent = colorbarParameter.getValue( 'ConstituentSelected', self.plotConstituents.keys()[0] )
            self.toggleColorbarVisibility( constituent, state )                       
            self.processConfigStateChange( config_function.value )
            
    def processColorbarConstituentSelection( self, *args, **kwargs ):
        #print " Process Colorbar Constituent Selection: %s  " % str( args )
        constituent = self.plotConstituents.keys()[ args[2] ]
        colorbarParameter = self.cfgManager.getParameter( 'Colorbar' )
        colorbarParameter.setValue( 'ConstituentSelected', constituent )
        self.toggleColorbarVisibility( constituent, 1 )                       

    def initializePlots(self):
        bbar = self.getPlotButtonbar()
        if not self.cfgManager.initialized:
            button = bbar.getButton( 'ZSlider' ) 
            if button <> None:
                button.setButtonState( 1 ) 
                bbar.initializeSliderPosition(0)  
        bbar.initializeState()
        
    def processChooseColormapCommand( self, args, config_function ):
        from ListWidget import ColorbarListWidget
        colormapParam = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            for plotItem in self.plotConstituents.items():
                self.setColormap( plotItem[0], config_function.initial_value )
        elif args and args[0] == "EndConfig":
            self.processConfigParameterChange( colormapParam )
        elif args and args[0] == "InitConfig":
            state = args[1]
            if ( self.colormapWidget == None ): #  or self.colormapWidget.checkWindowSizeChange():
                self.colormapWidget = ColorbarListWidget( self.renderWindowInteractor ) 
                bbar = args[3]
                self.colormapWidget.StateChangedSignal.connect( bbar.processInteractionEvent )
            if len( args ) == 1:    self.colormapWidget.toggleVisibility()
            else:                   self.colormapWidget.toggleVisibility( state = args[1] )
            if state: self.logoWidget.Off()
            else: self.logoWidget.On()
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            if self.colormapWidget <> None:
                self.colormapWidget.hide()
        elif args and args[0] == "UpdateConfig":
            cmap_data = args[3]
            for plotItem in self.plotConstituents.items():
                if self.isConstituentConfigEnabled(plotItem[0]):
                    self.setColormap( plotItem[0], cmap_data )
            colormapParam.setValues( cmap_data  )

    def isConstituentConfigEnabled(self, constituent ):
        param = None
        for plotItem in self.plotConstituents.items():
            if constituent == plotItem[0]: param = self.cfgManager.getParameter( plotItem[1] ) 
        return param.getValue( 'ConfigEnabled', True ) if ( param <> None ) else True

    def getInteractionState( self, key ):
        for bbar in ButtonBarWidget.getButtonBars():
            state = bbar.getInteractionState( key )
            if state[0] <> None: return state
        return ( None, None, None )    
            
    def displayEventType(self, caller, event):
        print " --> Event: %s " % event 
        return 0
    
    def updateAnimationControlBar(self, state, config_function ):
        bbar = self.getControlBar( config_function, [ ( "Step", "Run", "Stop" ), self.processAnimationStateChange ], mag=1.4 )
        if state == 1:
            #print " ** Displaying AnimationControlBar ** "
            self.updateTextDisplay( config_function.label )
            bbar.show()
            if self.animating:
                self.changeButtonActivations( [ ( 'Run', False ), ( 'Stop', True ) , ( 'Step', False ) ] )  
            else:
                self.changeButtonActivations( [ ( 'Run', True ), ( 'Stop', False ) , ( 'Step', True ) ] )  
        else:
            bbar.hide()
    
    def processAnimationCommand( self, args, config_function = None ):
#        print " processAnimationCommand, args = ", str( args ), ", animating = ", str(self.animating)
        runSpeed = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            pass
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            state = args[1]
            bbar = self.getControlBar( config_function, [ ( "Step", "Run", "Stop", ( "Record", True ) ), self.processAnimationStateChange ], mag=1.4 )
            if state == 1:
                self.updateTextDisplay( config_function.label )
                bbar.show()
                if self.animating:
                    self.changeButtonActivations( [ ( 'Run', False ), ( 'Stop', True ) , ( 'Step', False ) ] )  
                else:
                    self.changeButtonActivations( [ ( 'Run', True ), ( 'Stop', False ) , ( 'Step', True ), ( 'Record', True, 0 ) ] )  
            else:
                bbar.hide()
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            try:
                value = args[2].GetValue()
                runSpeed.setValue( 0, value )
            except Exception, err:
                print>>sys.stderr, "Error setting animation run speed: ", str( err )
                                   
    def processAnimationStateChange( self, button_id, key, state, force = False ):
#        print " Process Animation State Change[%s], state = %d " % ( button_id, state )
        if button_id == 'Step':
            self.stepAnimation()
        elif button_id == 'Run':
            if self.animationTimerId == -1: 
                self.changeButtonActivations( [ ( 'Run', False ), ( 'Stop', True ) , ( 'Step', False ) ] )  
                self.animationStepper.startAnimation()
                self.animating = True
        elif button_id == 'Stop':
            self.animationStepper.stopAnimation()
            self.animating = False
        elif button_id == 'Record':
            self.record_animation = state
            print " Set record_animation: " , str( self.record_animation )
            if self.record_animation == 0:
                self.saveAnimation()
          
    def startAnimation(self):   
        self.notifyStartAnimation()
        self.animating = True
        self.runAnimation()
                     
    def stopAnimation(self):
        self.animating = False
        if self.animationTimerId <> -1: 
            self.animationTimerId = -1
            self.renderWindowInteractor.DestroyTimer( self.animationTimerId  ) 
            self.saveAnimation()
            self.notifyStopAnimation()           
        
    def notifyStartAnimation(self): 
        pass
    
    def notifyStopAnimation(self): 
        self.changeButtonActivations( [ ( 'Run', True ), ( 'Stop', False ) , ( 'Step', True ) ] ) 
                           
    def setInteractionState(self, caller, event):
        interactor = caller.GetInteractor()
        key = interactor.GetKeyCode() 
        keysym = interactor.GetKeySym()
        shift = interactor.GetShiftKey()
#        print " setInteractionState -- Key Press: %c ( %d: %s ), event = %s " % ( key, ord(key), str(keysym), str( event ) )
        alt = ( keysym <> None) and keysym.startswith("Alt")
        if alt:
            self.isAltMode = True
        else:                
            self.processKeyEvent( key, caller, event )
        return 0

    def processKeyEvent( self, key, caller=None, event=None, **args ):
        interactor = caller.GetInteractor()
        keysym = interactor.GetKeySym() if caller else key
        ctrl = interactor.GetControlKey() if caller else args.get( 'ctrl', 0 )
        eventArgs = [ key, keysym, ctrl ] 
        if self.processKeyPressHandler( key, eventArgs ): 
            return 1
        else: 
            return self.onKeyEvent( eventArgs ) 
     
#         
#         if self.onKeyEvent( [ key, keysym, ctrl ] ):
#             pass
#         else:
#             ( state, persisted, guibar ) =  self.getInteractionState( key )
#     #            print " %s Set Interaction State: %s ( currently %s) " % ( str(self.__class__), state, self.InteractionState )
#             if state <> None: 
#                 print " ------------------------------------------ setInteractionState, key=%s, state = %s    ------------------------------------------ " % (str(key), str(state)  )
#                 guibar.updateInteractionState( state, **args  )                 
#                 self.isAltMode = False 
#                 
#         for button_bar in self.button_bars.values():
#             button_bar.processKeyEvent( keysym, ctrl )
#         return 0

    def onLeftButtonPress( self, caller, event ):
        return 0
#        istyle = self.renderWindowInteractor.GetInteractorStyle()
# #        print "(%s)-LBP: s = %s, nis = %s " % ( getClassName( self ), getClassName(istyle), getClassName(self.navigationInteractorStyle) )
#         if not self.finalizeLeveling(): 
# #            shift = caller.GetShiftKey()
#             self.currentButton = self.LEFT_BUTTON
#  #           self.clearInstructions()
#             self.UpdateCamera()   
#             x, y = caller.GetEventPosition()      
#             self.startConfiguration( x, y, [ 'leveling', 'generic' ] )  
#         return 0

    def onRightButtonPress( self, caller, event ):
#         shift = caller.GetShiftKey()
#         self.currentButton = self.RIGHT_BUTTON
#  #       self.clearInstructions()
#         self.UpdateCamera()
#         x, y = caller.GetEventPosition()
#         if self.InteractionState <> None:
#             self.startConfiguration( x, y,  [ 'generic' ] )
        return 0

    def onLeftButtonRelease( self, caller, event ):
        print " onLeftButtonRelease "
        self.currentButton = None
        self.recordCamera()
 
    
    def onRightButtonRelease( self, caller, event ):
        print " onRightButtonRelease "
        self.currentButton = None 
        self.recordCamera()


#     def updateLevelingEvent( self, caller, event ):
#         x, y = caller.GetEventPosition()
#         wsize = caller.GetRenderWindow().GetSize()
#         self.updateLeveling( x, y, wsize )
#                 
#     def updateLeveling( self, x, y, wsize, **args ):  
#         if self.configuring:
#             configFunct = self.configurableFunctions[ self.InteractionState ]
#             if configFunct.type == 'leveling':
#                 configData = configFunct.update( self.InteractionState, x, y, wsize )
#                 if configData <> None:
#                     self.setParameter( configFunct.name, configData ) 
#                     textDisplay = configFunct.getTextDisplay()
#                     if textDisplay <> None:  self.updateTextDisplay( textDisplay )
#                     self.render()

    def updateTextDisplay( self, text=None, render=False ):
        if text <> None:
            self.labelBuff = "%s" % str(text) 
        label_actor = self.getLabelActor()
        if label_actor:
            label_actor.ComputeScaledFont( self.renderer )     
            label_actor.VisibilityOn() 
        if render: self.render() 
        
    def getDisplayText(self): 
        return self.labelBuff   

    def getLabelActor(self):
        return self.textDisplayMgr.getTextActor( 'label', self.labelBuff, bold = False  ) if self.textDisplayMgr else None
    
    def UpdateCamera(self):
        pass
    
    def setParameter( self, name, value ):
        pass

    
    def getLut( self, constituent, cmap_index=0  ):
        colormapManager = self.getColormapManager( constituent, index=cmap_index )
        return colormapManager.lut
        
    def updatingColormap( self, cmap_index, colormapManager ):
        pass

    def addObserver( self, target, event, observer ):
        self.observerTargets.add( target ) 
        target.AddObserver( event, observer ) 

    def createRenderer(self, **args ):
        background_color = args.get( 'background_color', VTK_BACKGROUND_COLOR )
        self.renderer.SetBackground(*background_color)   
        self.textDisplayMgr = TextDisplayMgr( self.renderer ) 
        self.renderWindowInitSize = args.get( 'window_size', None ) 
        if self.renderWindowInitSize <> None:
            self.renderWindow.SetSize( self.renderWindowInitSize )                             
        self.pointPicker = vtk.vtkPointPicker()
        self.pointPicker.PickFromListOn()   
        try:        self.pointPicker.SetUseCells(True)  
        except:     print>>sys.stderr,  "Warning, vtkPointPicker patch not installed, picking will not work properly."
        self.pointPicker.InitializePickList()             
        self.renderWindowInteractor.SetPicker(self.pointPicker) 
        self.addObserver( self.renderer, 'ModifiedEvent', self.activateEvent )
        self.clipper = vtk.vtkBoxWidget()
        self.clipper.RotationEnabledOff()
        self.clipper.SetPlaceFactor( 1.0 ) 
        self.clipper.KeyPressActivationOff()
        self.clipper.SetInteractor( self.renderWindowInteractor )    
        self.clipper.SetHandleSize( 0.005 )
        self.clipper.SetEnabled( True )
        self.clipper.InsideOutOn()  
        self.clipper.AddObserver( 'StartInteractionEvent', self.startClip )
        self.clipper.AddObserver( 'EndInteractionEvent', self.endClip )
        self.clipper.AddObserver( 'InteractionEvent', self.executeClip )           
        self.clipOff() 
        self.addLogo()
    
    def clipOn(self):
        pass

    def clipOff(self):
        pass

    def startClip( self, caller=None, event=None ):
        pass

    def endClip( self, caller=None, event=None ):
        pass
        
    def executeClip( self, caller=None, event=None ):
        pass
    
    def isConfigStyle( self, iren ):
        return False
#         if not iren: return False
#         return getClassName( iren.GetInteractorStyle() ) == getClassName( self.configurationInteractorStyle )
            
    def onKeyRelease(self, caller, event):
        #print " Key event "
        return
        
    def onModified(self, caller, event):
#        print " --- Modified Event --- "
        return 0

    def onAnyEvent(self, caller, event):
        print " --- %s Event --- " % str(event)
        return 0    

    def onCameraEvent(self, caller, event):
        print " --- %s Event --- " % str(event)
        return 0    
    
    def onRender(self, caller, event):
#        print " --- Render Event --- "
#        traceback.print_stack()
        return 0
    
    def updateInteractor(self): 
        return 0    
 
    def activateEvent( self, caller, event ):
        if not self.activated:
            self.renderWindowInteractor.Initialize()
#            print "Activating, renderWindowInteractor = ", self.renderWindowInteractor.__class__.__name__
#            self.addObserver( self.renderWindowInteractor, 'InteractorEvent', self.displayEventType )                   
            self.addObserver( self.interactorStyle, 'CharEvent', self.setInteractionState )                   
            self.addObserver( self.renderWindowInteractor, 'TimerEvent', self.processTimerEvent )                   
#            self.addObserver( self.renderWindowInteractor, 'CreateTimerEvent', self.processTimerEvent )                   
#            self.addObserver( self.renderWindowInteractor, 'MouseMoveEvent', self.updateLevelingEvent )
            self.addObserver( self.interactorStyle, 'KeyReleaseEvent', self.onKeyRelease )
            self.addObserver( self.renderWindowInteractor, 'LeftButtonPressEvent', self.onLeftButtonPress )            
            self.addObserver( self.interactorStyle, 'ModifiedEvent', self.onModified )
            self.addObserver( self.renderWindowInteractor, 'RenderEvent', self.onRender )                   
            self.addObserver( self.renderWindowInteractor, 'LeftButtonReleaseEvent', self.onLeftButtonRelease )
            self.addObserver( self.renderWindowInteractor, 'RightButtonReleaseEvent', self.onRightButtonRelease )
            self.addObserver( self.renderWindowInteractor, 'RightButtonPressEvent', self.onRightButtonPress )
#            self.addObserver( self.renderWindowInteractor, 'RenderWindowMessageEvent', self.onAnyEvent )
#            self.addObserver( self.renderWindowInteractor, 'ResetCameraEvent', self.onCameraEvent )
#            self.addObserver( self.renderWindowInteractor, 'CameraEvent', self.onCameraEvent )
#            self.addObserver( self.renderWindowInteractor, 'ResetCameraClippingRangeEvent', self.onAnyEvent )
#            self.addObserver( self.renderWindowInteractor, 'ComputeVisiblePropBoundsEvent', self.onAnyEvent )
#            self.addObserver( self.renderWindowInteractor, 'AnyEvent', self.onAnyEvent )
            
#            self.animationTestId = self.renderWindowInteractor.CreateRepeatingTimer( 100 )
            
#            cb = TimerCallback()
#            self.renderWindowInteractor.AddObserver(vtk.vtkCommand.TimerEvent, cb)
            
            RenderWindow = self.renderWindowInteractor.GetRenderWindow()   
#            RenderWindow.AddObserver( 'AnyEvent', self.onAnyWindowEvent )
            RenderWindow.AddObserver( 'RenderEvent', self.onWindowRenderEvent )
            RenderWindow.AddObserver( 'ExitEvent', self.onWindowExit )
            self.updateInteractor()
            self.activated = True 
            
    def buildConfigurationButton(self):
        bbar_name = 'Configure'
        bbar = self.buttonBarHandler.getButtonBar( bbar_name )
        if bbar == None:
            bbar = self.buttonBarHandler.createButtonBarWidget( bbar_name, self.renderWindowInteractor )
            config_button = bbar.addConfigButton( names=['Configure'], id='Configure', key='g', toggle=True, persist=False, interactionHandler=self.processConfigurationToggle )
            bbar.build()
        return bbar

    def showConfigurationButton(self):
        bbar = self.buildConfigurationButton( )
        bbar.show()
        
    def buildPlotButtons( self, **args ):
        bbar_name = 'Plot'
        enable_3d_plots = True
        ispec = self.inputSpecs.get(  0 , None )
        if ispec is not None:
            md = ispec.metadata 
            plotType  = md.get( 'plotType', 'xyz' )
            lev = md.get( 'lev', None )
            if (lev is None) and (plotType == 'xyz'): enable_3d_plots = False
        bbar = self.buttonBarHandler.createButtonBarWidget( bbar_name, self.renderWindowInteractor, position=( 0.0, 0.96) )
        self.buttonBarHandler.DefaultGroup = 'SliceRoundRobin'
        if (self.type == '3d_vector') or not enable_3d_plots:
            sliderLabels= 'Slice Position' if enable_3d_plots else []
            b = bbar.addSliderButton( names=['ZSlider'],  key='z', toggle=True, group='SliceRoundRobin', sliderLabels=sliderLabels, label="Slicing", state = 1, interactionHandler=self.processSlicingCommand )
        else:
            b = bbar.addConfigButton( names=['SliceRoundRobin'],  key='p', interactionHandler=bbar.sliceRoundRobin )
            b = bbar.addSliderButton( names=['XSlider'],  key='x', toggle=True, group='SliceRoundRobin', sliderLabels='X Slice Position', label="Slicing", position=[0,3], interactionHandler=self.processSlicingCommand )
            b.addFunctionKey( 'W', 1, Button.FuncToggleStateOff )
            b = bbar.addSliderButton( names=['YSlider'],  key='y', toggle=True, group='SliceRoundRobin', sliderLabels='Y Slice Position', label="Slicing", position=[1,3], interactionHandler=self.processSlicingCommand )
            b.addFunctionKey( 'W', 1, Button.FuncToggleStateOff )
            b = bbar.addSliderButton( names=['ZSlider'],  key='z', toggle=True, group='SliceRoundRobin', sliderLabels='Z Slice Position', label="Slicing", position=[2,3], interactionHandler=self.processSlicingCommand )
            b.addFunctionKey( 'W', 1, Button.FuncToggleStateOff )
            b = bbar.addConfigButton( names=['ToggleSurfacePlot'],  key='S', children=['IsosurfaceValue'], toggle=True, interactionHandler=self.processSurfacePlotCommand )
            b = bbar.addConfigButton( names=['ToggleVolumePlot'], key='v', children=['ScaleTransferFunction'], toggle=True, interactionHandler=self.processVolumePlotCommand )
        bbar.build()

    def processSurfacePlotCommand( self, args, config_function = None ):
        if args and args[0] == "Init":
            state = config_function.getState()
            if state: self.cfgManager.initialized = True 
            if config_function.initial_value <> None:
                config_function.setState( config_function.initial_value[0] )
            state = config_function.getState()
            if state: self.toggleIsosurfaceVisibility( state )
        elif args and args[0] == "InitConfig":
            state = args[1]
            self.toggleIsosurfaceVisibility( state )
            self.processConfigStateChange( config_function.value )

    def processVolumePlotCommand( self, args, config_function = None ):
        if args and args[0] == "Init":
            state = config_function.getState()
            if state: self.cfgManager.initialized = True
            if config_function.initial_value <> None:
                config_function.setState( config_function.initial_value[0] )
#            print " init ToggleVolumePlot: state=%s, cfg=(%s) " % ( str(state), str(config_function.initial_value) )
            state = config_function.getState()
            if state: self.toggleVolumeVisibility( state )
        elif args and args[0] == "InitConfig":
            state = args[1]
            self.toggleVolumeVisibility( state )
            self.processConfigStateChange( config_function.value )

    
    def fetchPlotButtons( self ):
        bbar1 = self.buttonBarHandler.getButtonBar( 'Plot' )
        if bbar1 == None: bbar1 = self.buildPlotButtons()
        bbar2 = self.buttonBarHandler.getButtonBar( 'Interaction' )
        bbar2.build()
        return bbar1
    
    def getPlotButtonbar(self):
        return self.buttonBarHandler.getButtonBar( 'Plot' )
    
    def getPlotButtons( self, names ):
        bbar = self.buttonBarHandler.getButtonBar( bbar_name )
        return [ bbar.getButton( name ) for name in names ] if bbar is not None else []
    
    def toggleCongurationButtons(self, isVisible ):
        config_bbars = [ 'Plot', 'Interaction' ]
        for bbar_name in config_bbars:
            bbar = self.buttonBarHandler.getButtonBar( bbar_name )
            if bbar:
                if isVisible: bbar.show()
                else: bbar.hide()
    
    def processConfigurationToggle( self, args, config_function = None ):
        if args and args[0] == "Init":
#             name = config_function.name
#             bbar = self.buttonBarHandler.getButtonBar( name )
#             button = bbar.getButton( name )
            state = config_function.getState() 
            if state: self.toggleCongurationButtons( state )
        elif args[0] == "InitConfig":
            name = config_function.name
            bbar = self.buttonBarHandler.getButtonBar( name )
            button = bbar.getButton( name )
            self.toggleCongurationButtons( button.getState() )
         
    def processSlicingCommand( self, args, config_function = None ):
        pass

        
    def addInteractionButtons(self):
        bbar_name = 'Interaction'
        bbar = self.buttonBarHandler.createButtonBarWidget( bbar_name, self.renderWindowInteractor, position=( 0.0, 0.5) )
        return bbar
    
    def getInteractionButtons(self): 
        bbar = self.buttonBarHandler.getButtonBar( 'Interaction' )
        if bbar == None:  bbar = self.addInteractionButtons()
        return bbar
        
    def showInteractionButtons( self, **args ):
        bbar = self.buttonBarHandler.getButtonBar( 'Interaction' )
        bbar.build( **args )
        bbar.show() 
        
    def onWindowRenderEvent( self, caller=None, event=None ):
        renwin = self.renderWindow if (caller == None) else caller  
        window_size = renwin.GetSize()  
        if ( window_size <> self.renderWindowSize ):
            self.onRenderWindowResize()
            self.renderWindowSize = window_size
        time.sleep(0.0)
         
    def onWindowExit( self, caller=None, event=None ):
        #print "Window Event: ", event
        self.onClosing()
             
    def onAnyWindowEvent( self, caller=None, event=None ):
        pass
        #print "Window Event: ", event

                                       
#     def onWindowModified( self, caller=None, event=None ):
#         print "Window Modified Event "
#         renwin = self.renderWindow if (caller == None) else caller 
#         window_size = renwin.GetSize()
#         if window_size <> (0,0):
#             if self.renderWindowSize == None:
#                 if self.renderWindowInitSize <> None:
#                     self.renderWindowSize = self.renderWindowInitSize
#                     self.renderWindow.SetSize( self.renderWindowInitSize ) 
#                     self.onRenderWindowResize()
#                 else:
#                     self.renderWindowSize = window_size 
#                     self.onRenderWindowResize()               
#             elif ( self.renderWindowSize <> window_size ):
#                 self.renderWindowSize = window_size
#                 self.onRenderWindowResize()

            
    def onRenderWindowResize( self ):
        if not self.resizingWindow:
#            print " onRenderWindowResize, size = ", str( self.renderWindowSize )
            self.resizingWindow = True
            self.animation_frames = []
            if self.colormapWidget <> None:
                self.colormapWidget.hide()
                self.colormapWidget = None
                bbar = self.buttonBarHandler.getButtonBar( 'Interaction' )
                button = bbar.getButton( 'ChooseColormap' )
                button.setToggleState( 0 )
                self.logoWidget.On()
            self.updateTextDisplay()
            self.buttonBarHandler.repositionButtons()
            self.renderWindow.Modified()
            self.renderWindow.MakeCurrent()
            self.render()
            self.resizingWindow = False

    def clearReferrents(self):
        self.removeObservers()
        self.renderer = None
        self.renderWindowInteractor = None

    def removeObservers( self ): 
        for target in self.observerTargets:
            target.RemoveAllObservers()
        self.observerTargets.clear()

    def printInteractionStyle(self, msg ):
        print "%s: InteractionStyle = %s " % ( msg,  self.renderWindowInteractor.GetInteractorStyle().__class__.__name__ ) 

    def toggleLogoVisibility( self ):
        if self.logoRepresentation:
            self.logoVisible = not self.logoVisible
            if self.logoVisible: self.logoWidget.On()
            else: self.logoWidget.Off()
            self.renderWindow.Render() 

    def addLogo(self):
        if self.logoRepresentation == None:
            defaultLogoFile = os.path.join(sys.prefix,"share","vcs","uvcdat.png")
            reader = vtk.vtkPNGReader()
            reader.SetFileName( defaultLogoFile )
            reader.Update()
            logo_input = reader.GetOutput()
            self.logoRepresentation = vtk.vtkLogoRepresentation()
            self.logoRepresentation.SetImage(logo_input)
            self.logoRepresentation.ProportionalResizeOn ()
            self.logoRepresentation.SetPosition( 0.82, 0.0 )
            self.logoRepresentation.SetPosition2( 0.18, 0.08 )
            self.logoRepresentation.GetImageProperty().SetOpacity( 0.9 )
            self.logoRepresentation.GetImageProperty().SetDisplayLocationToBackground() 
            self.logoWidget = vtk.vtkLogoWidget()
            self.logoWidget.SetInteractor( self.renderWindowInteractor )
            self.logoWidget.SetRepresentation(self.logoRepresentation)
            self.logoWidget.SelectableOff()
            self.logoWidget.SetManagesCursor(0)
            self.logoWidget.SetResizable(0)
            self.logoWidget.On()
            self.render() 

    def createRenderWindow( self, **args ):
        blocking = args.get( 'blocking', False )
        renWin = args.get( 'renwin', None ) 
        if renWin == None:
            renWin = vtk.vtkRenderWindow()
            
        rendWinInteractor = renWin.GetInteractor()
        if rendWinInteractor == None:                
            rendWinInteractor = vtk.vtkRenderWindowInteractor() if blocking else vtk.vtkGenericRenderWindowInteractor()
            renWin.SetInteractor( rendWinInteractor )
            rendWinInteractor.SetRenderWindow(renWin)  
        
        self.renderWindowInteractor =  rendWinInteractor              
        self.renderer = vtk.vtkRenderer()
        renWin.AddRenderer( self.renderer )

        self.interactorStyle = vtk.vtkInteractorStyleTrackballCamera( )
        self.renderWindowInteractor.SetInteractorStyle( self.interactorStyle )
        self.interactorStyle.KeyPressActivationOff( )
        self.interactorStyle.SetEnabled(1)
                     
        if self.useDepthPeeling:
            self.renderer.UseDepthPeelingOn( )
            self.renderer.SetOcclusionRatio( 0.2 )       
            renWin.SetAlphaBitPlanes( 1 )
            renWin.SetMultiSamples( 0 )
            
        self.renderer.SetBackground(1.0, 1.0, 1.0)
        self.renderer.SetNearClippingPlaneTolerance( 0.0001 )    
        self.renderWindow = renWin
 
    def updateInteractionStyle(self):
        self.renderWindowInteractor.SetInteractorStyle( self.interactorStyle )
   
    def closeConfigDialog(self):
        print "x"
        pass
    
    def enableRender(self, **args ):
        return True

    def render( self, **args ):
        if self.enableRender( **args ):
            self.renderWindow.Render()

    def processEvent(self, eventArgs ):
        if eventArgs[0] == "KeyEvent":
            self.onKeyEvent( eventArgs[1:])
        if eventArgs[0] == "ResizeEvent":
            self.onResizeEvent()           
            
    def onKeyEvent(self, eventArgs ):
        key = eventArgs[0]
        keysym =  eventArgs[1]            
        if keysym   == "i":  self.clearInteractions()
        elif keysym == "s":  self.enableDualInputs()
        else: return False
        return True
    
    def enableDualInputs(self):
        pass

    def getLUT( self, constituent, cmap_index=0  ):
        colormapManager = self.getColormapManager( constituent, index=cmap_index )
        return colormapManager.lut

    def toggleColorbarVisibility(self, constituent, state ):
        for colormapManagerItem in self.colormapManagers.items():
            item_constituent = colormapManagerItem[0].split('-')[0]
            if state == 0 :
                colormapManagerItem[1].toggleColorbarVisibility( state=0 )
            else:
                const_state = 1 if (item_constituent == constituent) else 0
                colormapManagerItem[1].toggleColorbarVisibility( state=const_state )
        self.render()
    
    def getColormapManager( self, constituent, **args ):
        cmap_index = args.get('index',0)
        name = args.get('name',None)
        invert = args.get('invert',None)
        smooth = args.get('smooth',None)
        cmap_name = '-'.join( [ constituent, str(cmap_index) ] )
        cmap_mgr = self.colormapManagers.get( cmap_name, None )
        if cmap_mgr == None:
            lut = vtk.vtkLookupTable()
            cmap_mgr = ColorMapManager( lut ) 
            self.colormapManagers[ cmap_name ] = cmap_mgr
        if (invert <> None): cmap_mgr.invertColormap = invert
        if (smooth <> None): cmap_mgr.smoothColormap = smooth
        if name:             cmap_mgr.load_lut( name )
        return cmap_mgr
        
    def setColormap( self, constituent, data, **args ):
        try:
            colormapName = str(data[0])
            invertColormap = getBool( data[1] ) 
            cmap_index = args.get( 'index', 0 )
            metadata = self.getMetadata()
            var_name = metadata.get( 'var_name', '')
            var_units = metadata.get( 'var_units', '')
    #        self.updateStereo( enableStereo )
            colormapManager = self.getColormapManager( constituent, name=colormapName, invert=invertColormap, index=cmap_index, units=var_units )
            if( colormapManager.colorBarActor == None ): 
                cm_title = str.replace( "%s (%s)" % ( var_name, var_units ), " ", "\n" )
                cmap_pos = [ 0.9, 0.2 ] if (cmap_index==0) else [ 0.02, 0.2 ]
                self.renderer.AddActor( colormapManager.createActor( pos=cmap_pos, title=cm_title ) )
    #        colormapManager.setColorbarVisibility( show_colorBar )
            self.updatingColormap( cmap_index, colormapManager )
            self.render() 
            return True
        except Exception, err:
            print>>sys.stderr, "Error setting colormap: ", str(err)
        return False 
    
    def getUnits(self, var_index ):
        return ""
    
    def getMetadata(self):
        return self.metadata
    

    def updateStereo( self, enableStereo ):   
        if enableStereo:
            self.renderWindow.StereoRenderOn()
            self.stereoEnabled = 1
        else:
            self.renderWindow.StereoRenderOff()
            self.stereoEnabled = 0

            
    def getColormap(self, constituent, cmap_index = 0 ):
        colormapManager = self.getColormapManager( constituent, index=cmap_index )
        return [ colormapManager.colormapName, colormapManager.invertColormap, self.stereoEnabled ]

    def start( self, block = False ):
        self.showConfigurationButton()
        self.renderWindow.Render()
        if block:  self.renderWindowInteractor.Start()
         
    def invalidate(self):
        self.isValid = False

#     def startEventLoop(self):
#         self.renderWindowInteractor.Start()

    def recordCamera( self ):
        c = self.renderer.GetActiveCamera()
        self.cameraOrientation[ self.topo ] = ( c.GetPosition(), c.GetFocalPoint(), c.GetViewUp() )
        cameraSpecs = self.cameraOrientation[ self.topo ] 
        cameraParameter = self.cfgManager.getParameter( 'Camera' )
        cameraParameter.setValue( 'Position', cameraSpecs[0] )
        cameraParameter.setValue( 'FocalPoint', cameraSpecs[1] )
        cameraParameter.setValue( 'ViewUp', cameraSpecs[2] )
        self.processConfigParameterChange( cameraParameter )

    def resetCamera( self, pts = None, **args ):
        cdata = self.cameraOrientation.get( self.topo, None )
        if cdata:
            self.renderer.GetActiveCamera().SetPosition( *cdata[0] )
            self.renderer.GetActiveCamera().SetFocalPoint( *cdata[1] )
            self.renderer.GetActiveCamera().SetViewUp( *cdata[2] )       
        elif pts:
            self.renderer.ResetCamera( pts.GetBounds() )
        else:
            self.renderer.ResetCamera( self.getBounds() )
            
    def initCamera(self, d=None, center = None ):
#        print " -------------------------- >>>>> --------------------------- >>>>  initCamera:  ", str( ( self.xcenter, self.ycenter, d ) )            
        cameraParameter = self.cfgManager.getParameter( 'Camera' )
        cPosition = cameraParameter.getValue( 'Position', None )
        if cPosition == None:
            if self.renderWindowSize <> None: 
                self.renderWindow.SetSize( self.renderWindowSize )  
            if d == None:
                mapSize = self.mapManager.map_cut_size
                d = ( mapSize[0] + mapSize[1] )
            if center == None:  center = self.mapManager.mapCenter
            self.renderer.GetActiveCamera().SetPosition( center[0], center[1], d )
            self.renderer.GetActiveCamera().SetFocalPoint( center[0], center[1], 0.0 )
            self.renderer.GetActiveCamera().SetViewUp( 0, 1, 0 )  
        else:
            cFocalPoint  = cameraParameter.getValue( 'FocalPoint', None )
            cViewUp      = cameraParameter.getValue( 'ViewUp', None )
            self.renderer.GetActiveCamera().SetPosition( *cPosition )
            self.renderer.GetActiveCamera().SetFocalPoint( *cFocalPoint )
            self.renderer.GetActiveCamera().SetViewUp( *cViewUp )  
        self.renderer.ResetCameraClippingRange() 
        self.printCameraPos( 'initCamera' )
            
    def getCamera(self):
        return self.renderer.GetActiveCamera()
    
    def setFocalPoint( self, fp ):
        self.renderer.GetActiveCamera().SetFocalPoint( *fp )
        
    def printCameraPos( self, label = "" ):
        cam = self.getCamera()
        cpos = cam.GetPosition()
        cfol = cam.GetFocalPoint()
        cup = cam.GetViewUp()
        camera_pos = (cpos,cfol,cup)
        print "%s: Camera => %s " % ( label, str(camera_pos) )

    def update(self):
        pass

    def getColormapSpec(self, constituent, cmap_index=0): 
        colormapManager = self.getColormapManager( constituent, index=cmap_index )
        spec = []
        spec.append( colormapManager.colormapName )
        spec.append( str( colormapManager.invertColormap ) )
        value_range = colormapManager.lut.GetTableRange() 
        spec.append( str( value_range[0] ) )
        spec.append( str( value_range[1] ) ) 
#        print " %s -- getColormapSpec: %s " % ( self.getName(), str( spec ) )
        return ','.join( spec )

    def onKeyPress( self, caller, event ):
        key = caller.GetKeyCode() 
        keysym = caller.GetKeySym()
        print " -- Key Press: %s ( %s ), event = %s " % ( key, str(keysym), str( event ) )
        if keysym == None: return
        alt = ( keysym.lower().find('alt') == 0 )
        ctrl = caller.GetControlKey() 
        shift = caller.GetShiftKey() 

#     def finalizeLeveling( self, cmap_index=0 ):
#         if self.configuring: 
#             self.finalizeConfigurationObserver( self.InteractionState )            
#             self.resetNavigation()
#             self.configuring = False
#             self.InteractionState = None
#             return True
#         return False
# #            self.updateSliceOutput()

