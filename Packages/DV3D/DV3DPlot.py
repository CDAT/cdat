'''
Created on Apr 30, 2014

@author: tpmaxwel
'''
from ColorMapManager import *
from ButtonBarWidget import *
import vtk, traceback
MIN_LINE_LEN = 50
VTK_NOTATION_SIZE = 10

class TextDisplayMgr:
    
    def __init__( self, renderer ):
        self.renderer = renderer
    
    def setTextPosition(self, textActor, pos, size=[400,30] ):
#        vpos = [ 2, 2 ] 
        vp = self.renderer.GetSize()
        vpos = [ pos[i]*vp[i] for i in [0,1] ]
        textActor.GetPositionCoordinate().SetValue( vpos[0], vpos[1] )      
        textActor.GetPosition2Coordinate().SetValue( vpos[0] + size[0], vpos[1] + size[1] )      
  
    def getTextActor( self, aid, text, pos, **args ):
        if text == None: return
        textActor = self.getProp( 'vtkTextActor', aid  )
        if textActor == None:
            textActor = self.createTextActor( aid, **args  )
            self.renderer.AddViewProp( textActor )
        self.setTextPosition( textActor, pos )
        text_lines = text.split('\n')
        linelen = len(text_lines[-1])
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
        textActor.SetTextScaleMode( vtk.vtkTextActor.TEXT_SCALE_MODE_PROP )  
        textActor.SetMaximumLineHeight( 0.05 )       
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
        textActor.GetPositionCoordinate().SetCoordinateSystemToDisplay()
        textActor.GetPosition2Coordinate().SetCoordinateSystemToDisplay() 
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
 
    def __init__( self,  **args ):
        self.type = args.get( 'gmname', 'default').lower()
        self.useDepthPeeling = False
        self.labelBuff = ""
        self.textDisplayMgr = None
        self.createRenderWindow( **args ) 
        self.cameraOrientation = {}
        self.maxStageHeight = 100.0
        self.observerTargets = set()
        self.xcenter = 100.0
        self.xwidth = 300.0
        self.ycenter = 0.0
        self.ywidth = 180.0
        
        self.configuring = False
        self.activated = False
#        self.buttons = {}
        self.renderWindowSize = None

        self.isAltMode = False
        self.createColormap = True
        self.colormapManagers= {}
        self.colormapWidget = None 
        self.colormapWindowSize = None
        self.keyPressHandlers = {}
        interactionButtons = self.getInteractionButtons()
        interactionButtons.addSliderButton( names=['VerticalScaling'], key='Z', toggle=True, label='Vertical Scaling', sliderLabels='Vertical Scale', interactionHandler=self.processVerticalScalingCommand, range_bounds=[ 0.1, 10.0 ], initValue= 1.0 )
        interactionButtons.addConfigButton( names=['ChooseColormap'], key='m', toggle=True, interactionHandler=self.processChooseColormapCommand, initValue=[ 'jet', False, False ]  )
        interactionButtons.addConfigButton( names=['ToggleClipping'], key='X', toggle=True, parents=['ToggleVolumePlot', 'ToggleSurfacePlot'], interactionHandler=self.processToggleClippingCommand  )
        interactionButtons.addConfigButton( names=['Colorbar'], key='b', toggle=True, label='Show Colorbar', interactionHandler=self.processShowColorbarCommand )
        self.addKeyPressHandler( 'r', self.resetCamera )
        self.addKeyPressHandler( 'q',  self.quit )
        self.addKeyPressHandler( 'Q',  self.quit )
        self.addKeyPressHandler( 's',  self.saveState )
        
    def addKeyPressHandler( self, key, handler ):
        handlers = self.keyPressHandlers.setdefault( key, [] )
        handlers.append( handler )
        
    def quit( self, **args ):
        eventArgs = args.get( 'args', None )
        if eventArgs and ( eventArgs[1] == 'Q' ):
            self.saveState()
        self.renderWindowInteractor.TerminateApp() 
            
    def saveState(self, **args): 
        print "Save State" 
        CfgManager.saveState()

    def processKeyPressHandler( self, key, eventArgs ):
        handlers = self.keyPressHandlers.get( key, [] )
        for handler in handlers: handler( args=eventArgs )
        return len( handlers )
            
    def processVerticalScalingCommand( self, args, config_function ):
        pass 
    
    def processToggleClippingCommand( self, args, config_function ):
        pass 
     
    def getRenderer(self):
        return self.renderWindow.GetRenderers().GetFirstRenderer ()

    def processShowColorbarCommand( self, args, config_function = None ):
        if args and args[0] == "InitConfig":
            self.toggleColorbarVisibility(state=args[1])                       
            self.render() 

    def initializePlots(self):
#         bbar = ButtonBarWidget.getButtonBar( 'Plot' )
#         button = bbar.getButton( 'XSlider' ) 
#         button.setButtonState( 1 ) 
#         bbar.initializeSliderPosition(0)  
        bbar = ButtonBarWidget.getButtonBar( 'Plot' )
        bbar.initializeState()
        self.render()
        
    def processChooseColormapCommand( self, args, config_function ):
        from ListWidget import ColorbarListWidget
        colormapParam = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            self.setColormap( config_function.initial_value )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            if ( self.colormapWidget == None ): #  or self.colormapWidget.checkWindowSizeChange():
                self.colormapWidget = ColorbarListWidget( self.renderWindowInteractor ) 
                bbar = args[3]
                self.colormapWidget.StateChangedSignal.connect( bbar.processInteractionEvent )
            if len( args ) == 1:    self.colormapWidget.toggleVisibility()
            else:                   self.colormapWidget.toggleVisibility( state = args[1] )
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            self.colormapWidget.hide()
        elif args and args[0] == "UpdateConfig":
            cmap_data = args[3]
            self.setColormap( cmap_data )
            colormapParam.setValues( cmap_data  )

    def getInteractionState( self, key ):
        for bbar in ButtonBarWidget.getButtonBars():
            state = bbar.getInteractionState( key )
            if state[0] <> None: return state
        return ( None, None, None )    
            
    def displayEventType(self, caller, event):
        print " --> Event: %s " % event 
        return 0
        
    def processTimerEvent(self, caller, event):
#        id0 = caller.GetTimerEventId ()
        return 0
#         id1 = caller.GetTimerEventType ()
#         id2 = caller.GetTimerEventPlatformId ()
#        print "TimerEvent: %d %d %d " % (  id0, id1, id2 )
        
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
            return self.onKeyEvent( ) 
     
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
        self.currentButton = None 
    
    def onRightButtonRelease( self, caller, event ):
        self.currentButton = None 


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
        if label_actor: label_actor.VisibilityOn() 
        if render: self.render() 
        
    def getDisplayText(self): 
        return self.labelBuff   

    def getLabelActor(self):
        return self.textDisplayMgr.getTextActor( 'label', self.labelBuff, (.18, .95), bold = False  ) if self.textDisplayMgr else None
    
    def UpdateCamera(self):
        pass
    
    def setParameter( self, name, value ):
        pass

    
    def getLut( self, cmap_index=0  ):
        colormapManager = self.getColormapManager( index=cmap_index )
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
        return 0
        
    def onModified(self, caller, event):
#        print " --- Modified Event --- "
        return 0

    def onAnyEvent(self, caller, event):
        print " --- %s Event --- " % event
        return 0    
    
    def onRender(self, caller, event):
        return 0
    
    def updateInteractor(self): 
        return 0    
 
    def activateEvent( self, caller, event ):
        if not self.activated:
#            self.addObserver( self.renderWindowInteractor, 'InteractorEvent', self.displayEventType )                   
            self.addObserver( self.interactorStyle, 'CharEvent', self.setInteractionState )                   
            self.addObserver( self.renderWindowInteractor, 'TimerEvent', self.processTimerEvent )                   
#            self.addObserver( self.renderWindowInteractor, 'MouseMoveEvent', self.updateLevelingEvent )
            self.addObserver( self.interactorStyle, 'KeyReleaseEvent', self.onKeyRelease )
            self.addObserver( self.renderWindowInteractor, 'LeftButtonPressEvent', self.onLeftButtonPress )            
            self.addObserver( self.interactorStyle, 'ModifiedEvent', self.onModified )
            self.addObserver( self.renderWindowInteractor, 'RenderEvent', self.onRender )                   
            self.addObserver( self.renderWindowInteractor, 'LeftButtonReleaseEvent', self.onLeftButtonRelease )
            self.addObserver( self.renderWindowInteractor, 'RightButtonReleaseEvent', self.onRightButtonRelease )
            self.addObserver( self.renderWindowInteractor, 'RightButtonPressEvent', self.onRightButtonPress )
#            self.addObserver( self.renderWindowInteractor, 'RenderWindowMessageEvent', self.onAnyEvent )
#            self.addObserver( self.renderWindowInteractor, 'ResetCameraEvent', self.onAnyEvent )
#            self.addObserver( self.renderWindowInteractor, 'ResetCameraClippingRangeEvent', self.onAnyEvent )
#            self.addObserver( self.renderWindowInteractor, 'ComputeVisiblePropBoundsEvent', self.onAnyEvent )
#            self.addObserver( self.renderWindowInteractor, 'UpdateSizeEvent', self.onAnyEvent )
            renWin = self.renderWindowInteractor.GetRenderWindow()   
            renWin.AddObserver( 'ModifiedEvent', self.onWindowModified )
            self.updateInteractor() 
            self.activated = True 
            
    def showConfigurationButton(self):
        bbar_name = 'Configure'
        bbar = ButtonBarWidget.getButtonBar( bbar_name )
        if bbar == None:
            bbar = ButtonBarWidget( bbar_name, self.renderWindowInteractor )
            config_button = bbar.addConfigButton( names=['Configure'], id='Configure', key='g', toggle=True, persist=False, interactionHandler=self.processConfigurationToggle )
#            config_button.StateChangedSignal.connect( self.togglePlotButtons )
            bbar.build()
        bbar.show()

    def buildPlotButtons(self):
        bbar_name = 'Plot'
        bbar = ButtonBarWidget( bbar_name, self.renderWindowInteractor, position=( 0.0, 0.96) )
        ButtonBarWidget.DefaultGroup = 'SliceRoundRobin'
        if self.type == 'vector':
            b = bbar.addSliderButton( names=['ZSlider'],  key='z', toggle=True, group='SliceRoundRobin', sliderLabels='Slice Position', label="Slicing", state = 1, interactionHandler=self.processSlicingCommand )            
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
        if args and args[0] == "InitConfig": 
            self.toggleIsosurfaceVisibility( args, config_function ) 

    def processVolumePlotCommand( self, args, config_function = None ):
        if args and args[0] == "InitConfig": 
            self.toggleVolumeVisibility( args, config_function )  
    
    def fetchPlotButtons( self, show = False ):
        bbar1 = ButtonBarWidget.getButtonBar( 'Plot' )
        if bbar1 == None: self.buildPlotButtons()
        if show:
            bbar1.show()
            self.showInteractionButtons()
        else:
            bbar2 = ButtonBarWidget.getButtonBar( 'Interaction' )
            bbar2.build()
        return bbar1
    
    def getPlotButtons( self, names ):
        bbar = self.fetchPlotButtons()
        return [ bbar.getButton( name ) for name in names ]
    
    def toggleCongurationButtons(self, isVisible ):
        config_bbars = [ 'Plot', 'Interaction' ]
        for bbar_name in config_bbars:
            bbar = ButtonBarWidget.getButtonBar( bbar_name )
            if bbar:
                if isVisible: bbar.show()
                else: bbar.hide()
    
    def processConfigurationToggle( self, args, config_function = None ):
        if args[0] == "InitConfig":
            name = config_function.name
            bbar = ButtonBarWidget.getButtonBar( name )
            button = bbar.getButton( name )
            self.toggleCongurationButtons( button.getState() )
         
    def processSlicingCommand( self, args, config_function = None ):
        pass

        
    def addInteractionButtons(self):
        bbar_name = 'Interaction'
        bbar = ButtonBarWidget( bbar_name, self.renderWindowInteractor, position=( 0.0, 0.5) )
        return bbar
    
    def getInteractionButtons(self): 
        bbar = ButtonBarWidget.getButtonBar( 'Interaction' )
        if bbar == None:  bbar = self.addInteractionButtons()
        return bbar
        
    def showInteractionButtons( self, **args ):
        bbar = ButtonBarWidget.getButtonBar( 'Interaction' )
        bbar.build( **args )
        bbar.show() 
             
    def onWindowModified( self, caller, event ):
        renwin = caller
        window_size = renwin.GetSize()
        if ( self.renderWindowSize == None ) or ( self.renderWindowSize <> window_size ):
            if self.renderWindowSize <> None: 
                self.onRenderWindowResize()
            self.renderWindowSize = window_size
            
    def onRenderWindowResize( self ):
        self.updateTextDisplay()
        ButtonBarWidget.repositionButtons()
        self.render()

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

    def createRenderWindow( self, **args ):
        blocking = args.get( 'blocking', False )
        renWin = args.get( 'renwin', None ) 
        if renWin == None:
            renWin = vtk.vtkRenderWindow()
            rendWinInteractor = vtk.vtkRenderWindowInteractor() if blocking else vtk.vtkGenericRenderWindowInteractor()
            renWin.SetInteractor( rendWinInteractor )
            rendWinInteractor.SetRenderWindow(renWin)  
                        
        self.renderer = vtk.vtkRenderer()
        renWin.AddRenderer( self.renderer )
        self.renderWindowInteractor = renWin.GetInteractor()

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
        self.renderWindow = renWin
    
    def closeConfigDialog(self):
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
#        elif keysym == "2":  self.enableDualInputs()
        else: return False
        return True
    
    def enableDualInputs(self):
        pass

    def getLUT( self, cmap_index=0  ):
        colormapManager = self.getColormapManager( index=cmap_index )
        return colormapManager.lut

    def toggleColorbarVisibility(self,**args):
        for colormapManager in self.colormapManagers.values():
            colormapManager.toggleColorbarVisibility(**args)
        self.render()
    
    def getColormapManager( self, **args ):
        cmap_index = args.get('index',0)
        name = args.get('name',None)
        invert = args.get('invert',None)
        smooth = args.get('smooth',None)
        cmap_mgr = self.colormapManagers.get( cmap_index, None )
        if cmap_mgr == None:
            lut = vtk.vtkLookupTable()
            cmap_mgr = ColorMapManager( lut ) 
            self.colormapManagers[cmap_index] = cmap_mgr
        if (invert <> None): cmap_mgr.invertColormap = invert
        if (smooth <> None): cmap_mgr.smoothColormap = smooth
        if name:   cmap_mgr.load_lut( name )
        return cmap_mgr
        
    def setColormap( self, data, **args ):
        try:
            colormapName = str(data[0])
            invertColormap = getBool( data[1] ) 
            cmap_index = args.get( 'index', 0 )
            metadata = self.getMetadata()
            var_name = metadata.get( 'var_name', '')
            var_units = metadata.get( 'var_units', '')
    #        self.updateStereo( enableStereo )
            colormapManager = self.getColormapManager( name=colormapName, invert=invertColormap, index=cmap_index, units=var_units )
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

            
    def getColormap(self, cmap_index = 0 ):
        colormapManager = self.getColormapManager( index=cmap_index )
        return [ colormapManager.colormapName, colormapManager.invertColormap, self.stereoEnabled ]

    def start( self, block = False ):
        self.renderWindowInteractor.Initialize()
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
            
    def initCamera(self, d = 400.0 ):
        self.renderer.GetActiveCamera().SetPosition( self.xcenter, self.ycenter, d )
        self.renderer.GetActiveCamera().SetFocalPoint( self.xcenter, self.ycenter, 0.0 )
        self.renderer.GetActiveCamera().SetViewUp( 0, 1, 0 )  
        self.renderer.ResetCameraClippingRange() 
        
    def adjustCamera( self, center, distance ): 
        self.xcenter = center[0]
        self.ycenter = center[1]
        self.initCamera( distance )
            
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

    def getColormapSpec(self, cmap_index=0): 
        colormapManager = self.getColormapManager( index=cmap_index )
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

