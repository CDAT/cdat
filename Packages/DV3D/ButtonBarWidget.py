'''
Created on May 28, 2014

@author: tpmaxwel
'''

import vtk, os, sys, collections
import numpy as np
from ConfigurationFunctions import *

PackagePath = os.path.dirname( __file__ )  
DataDir = os.path.join( PackagePath, 'data' )
ButtonDir = os.path.join( DataDir, 'buttons' )

class OriginPosition:
    Upper_Left = [ 0, 1 ] 
    Upper_Right = [ 1, 1 ]  
    Lower_Left = [ 0, 0 ]  
    Lower_Right = [ 1, 0 ] 
    
class Orientation:
    Horizontal = 0 
    Vertical = 1 

class ProcessMode:
    Default = 0
    Slicing = 1
    Thresholding = 2
    LowRes = 0
    HighRes = 1
    AnyRes = 2
        
class Button:
    
    FuncToggleStateOn = 1
    FuncToggleStateOff = 2
        
    def __init__( self, iren, **args ):
        self.PublicStateChangedSignal = SIGNAL('PublicStateChangedSignal')
        self.PrivateStateChangedSignal = SIGNAL('PrivateStateChangedSignal')
        self.invokingEvent = False
        self.active = True
        self.renderWindowInteractor = iren
        self.names = args.get( 'names', [] )
        self._state = args.get( 'state', 0 )
        self.children = args.get( 'children', [] )
        self.toggle = args.get( 'toggle', False )
        self.parents = args.get( 'parents', [] )
        self.numberOfStates = args.get( 'nstates', ( 2 if self.toggle else len( self.names ) ) )
        self.id = args.get( 'id', self.names[0] if self.numberOfStates else None )
        self.key = args.get( 'key', None )
        self.image_size = None
        self.numberOfImages = 0
        self.button_files = [ ]
        self.functionKeys = { }
        self.createButtonRepresentation()
        self.buttonWidget = vtk.vtkButtonWidget()
        self.buttonWidget.SetInteractor(self.renderWindowInteractor)
        self.buttonWidget.SetRepresentation( self.buttonRepresentation )
        self.buttonWidget.AddObserver( 'StateChangedEvent', self.processStateChangeEvent )
#        self.buttonWidget.AddObserver( 'AnyEvent', self.processButtonEvent )
        self.buttonRepresentation.Highlight( self._state )
        self.buttonWidget.SetPriority(1.0)
        self.updateWidgetState()
#         if self.id == 'ToggleVolumePlot':
#             print "."

    def processButtonEvent(self, *args ):
        print '-----> processButtonEvent: ', str( args )

    def getState(self):
        return self._state

    def setState(self, value):
#        print "----------------->>> Button [%s] Setting state = %s " % ( self.id, str(value) )
        self._state = value
        self.buttonRepresentation.Highlight( self._state )
        self.setToggleProps()
        self.PrivateStateChangedSignal( value )

#         if self.id == 'ToggleVolumePlot':
#             print "."
#         if value == 1:
#             print "."
        
    def getFunctionMapKey (self, key, ctrl ):
        return "c-%c" % key if ctrl else "%c" % key

    def addFunctionKey(self, key, ctrl, function ):
        fkey =  self.getFunctionMapKey( key, ctrl )       
        self.functionKeys[ fkey ]  = function
        
    def processFunctionKey( self, key, ctrl ):
        fkey =  self.getFunctionMapKey( key, ctrl )       
        function = self.functionKeys.get( fkey, None )
        if function <> None: 
            self.executeFunction( function )
            return 1
        return 0
    
    def updateWidgetState(self): 
        if self.numberOfImages > 1:
            if self.numberOfImages <> self.numberOfStates:
                print>>sys.stderr, "Error, mismatch between numberOfImages(%d) and numberOfStates(%d) in Button %s " % ( self.numberOfImages, self.numberOfStates, self.id )
            else:
                self.buttonRepresentation.Modified()
                self.buttonRepresentation.NeedToRenderOn()
                self.buttonWidget.Render()
#                print "Button %s: widget state = %d " % ( self.id, self.buttonRepresentation.GetState() )
    
    def executeFunction( self, function ):
        if   function == Button.FuncToggleStateOn:  self.setToggleState( 1 )
        elif function == Button.FuncToggleStateOff: self.setToggleState( 0 )
    
    def createButtonRepresentation(self, **args):
        self.buttonRepresentation = vtk.vtkTexturedButtonRepresentation2D()
        self.buttonRepresentation.SetPlaceFactor( args.get( 'scale', 1 ) )
        self.numberOfImages = len( self.names )
        if self.numberOfImages:
            self.buttonRepresentation.SetNumberOfStates(self.numberOfImages)
            for button_index in range( self.numberOfImages ):                
                buttonFilePath = os.path.join( ButtonDir,  '.'.join( [ self.names[ button_index ], 'jpeg' ] ) )
                JPEGReader = vtk.vtkJPEGReader()
                JPEGReader.SetFileName ( buttonFilePath )
                JPEGReader.Update()
                image_data = JPEGReader.GetOutput()
                if self.image_size == None: self.image_size = image_data.GetDimensions()
                self.buttonRepresentation.SetButtonTexture( button_index, image_data )
                self.button_files.append( buttonFilePath )
            self.setToggleProps()
        
    def addObserver(self, observer, **args ):
        event = args.get( 'event', 'StateChangedEvent' )
        self.buttonWidget.AddObserver( event, observer )
        
    def setToggleProps(self, state = None ):
        if self.toggle:
            prop = self.buttonRepresentation.GetProperty()
            opacity = 0.4 if ( self.getState() == 0 ) else 1.0
            prop.SetOpacity( opacity )
            self.buttonRepresentation.SetProperty(prop)
            prop = self.buttonRepresentation.GetHoveringProperty()
            h_opacity = 0.7 if ( self.getState() == 0 ) else 1.0
            prop.SetOpacity( h_opacity )
            self.buttonRepresentation.SetHoveringProperty(prop)
            self.updateWidgetState()
     #       print "  ---> setToggleProps[%s:%x]: active = %s " % ( self.id, id(self), str( self.getState() ) )
            
    def processKeyEvent( self, key, ctrl = 0 ):
        if self.processFunctionKey( key, ctrl ): 
            return True        
        if key == self.key and not self.invokingEvent:
            self.buttonRepresentation.Highlight( self.buttonRepresentation.HighlightSelecting )
            self.processStateChangeEvent( self, "KeyEvent" )
            self.buttonRepresentation.Highlight( self.buttonRepresentation.HighlightNormal )
            return True
        return False
    
    def setToggleState( self, state ):
#        print "Button[%s]:setToggleState(%d)" % ( self.id, state )
        self.setState(state)
        self.setToggleProps()       

    def processStateChangeEvent( self, obj, event, indirect = False ):
        self.invokingEvent = True 
#        print "Button[%s]:processStateChangeEvent(%d)" % ( self.id, self.getState() )
        self.setButtonState( ( self.getState() + 1 ) % self.numberOfStates )
        self.invokingEvent = False
        
    def refreshButtonState(self):
#        print "Button[%s]:refreshButtonState(%d)" % ( self.id, self.getState() )
        state = self.getState()
        self.broadcastState( state )
        self.setToggleProps()
        
    def setButtonState( self, state, broadcast = True ):
#        print "Button[%s]:setButtonState(%d)" % ( self.id, state )
        if (state <> self.getState()) or not self.toggle:
            if broadcast:
                self.broadcastState( state )
            self.setState(state)
    #         if (self.key <> None) and not indirect:
    #             self.renderWindowInteractor.SetKeyEventInformation( 0, 0, self.key, 1, self.key )
    #             self.renderWindowInteractor.InvokeEvent( 'CharEvent' )
            self.setToggleProps()
        
    def broadcastState(self, state ):
        self.PublicStateChangedSignal( self.id, self.key, state )
        
    def place( self, bounds ):
 #       print " Place Button %s: %s " % ( self.id, str( bounds ) )
        self.buttonRepresentation.PlaceWidget( bounds )
        
    def size(self):
        return self.image_size
    
    def On(self):
        if self.active:
            self.buttonWidget.On()
#            print " Button %s on " % self.id

    def Off(self):
        self.buttonWidget.Off()
#         if self.id == "Step":
#             print " Button %s off " % self.id
        
    def activate(self):
        self.active = True
        self.buttonWidget.On()
#        print " Button %s on " % self.id

    def deactivate(self):
        self.active = False
        self.Off()
                    
class ButtonBarHandler:
    
    def __init__( self, cfgMgr, **args ):
        self.current_configuration_mode = None
        self.button_bars = {}
        self.DefaultGroup = None
        self.cfgManager = cfgMgr             

    def hideWidgets(self ):
        for bbar in self.button_bars.values():
            bbar.setVisibility(False)

    def showWidgets(self ):
        for bbar in self.button_bars.values():
            bbar.setVisibility(True)

    def createButtonBarWidget( self, name, interactor, **args  ):
        bbar = self.getButtonBar( name )
        if bbar == None:
            bbar = ButtonBarWidget( self, name, interactor, **args  )
            self.button_bars[ name ] = bbar
        return bbar

    def createControlBar( self, name, interactor, build_args, **args ):
        cbar = self.getButtonBar( name )
        if cbar == None:
            if 'position' not in args:      args[ 'position' ]    = ( 0.55, 0.08 ) 
            if 'orientation' not in args:   args[ 'orientation' ] = Orientation.Horizontal
            cbar = ControlBar( name, interactor, **args )
            cbar.init( build_args, **args )
     #       print "Activating *constituent* buttons, cbar '%s':%x" % ( name, id(cbar) )
            for button in cbar.buttons:
                 button.activate()
                 button.setToggleState( 1 )
            self.button_bars[ name ] = cbar
        return cbar

    def getButtonBar( self, name ):
        return self.button_bars.get( name, None )
      
    def broadcastButtonState( self, type, name,  **args ):
        bbar = self.getButtonBar( type )
        button = bbar.getButton( name )    
        button.broadcastState( **args )    
 
    def getButtonBars( self ):
        return self.button_bars.values()
 
    def findButton( self, name  ):
        for bbar in self.button_bars.values():
            b = bbar.getButton( name )
            if b <> None: return b
        return None
      
    def repositionButtons( self ):
        for button_bar in self.button_bars.values():
            button_bar.reposition()                                          
  
    def initializeConfigurations( self, **args ) :
        for bbar in self.button_bars.values():
            bbar.initializeConfiguration( **args )
        for bbar in self.button_bars.values():
            bbar.initializeChildren( **args )
 
    def restoreInteractionState( self, state ): 
#        print "  ----------------------------- restoreInteractionState ----------------------------- " 
        bbar = self.getButtonBar( 'Plot' ) 
        if bbar is not None:
            bbar.InteractionState = None
            current_config_function = None
            for configFunct in bbar.configurableFunctions.values():
                if ( configFunct.type == 'slider' ) and ( configFunct.active ) and ( configFunct.group == self.DefaultGroup ):
                    b = bbar.getButton( configFunct.name )
                    if b.getState():
    #                    print "Activating Slider ", configFunct.name
                        if current_config_function == None:
                            current_config_function = configFunct
                        else:
                            if not configFunct.sameGroup( current_config_function ):
                                print>>sys.stderr, "Error, interaction state conflict: %s vs %s " % ( configFunct.name, bbar.InteractionState) 
                                return
                        bbar.InteractionState = configFunct.name 
#                        print " ---> Set Plot bbar interaction State: ", configFunct.name
                        n_active_sliders = configFunct.position[1] if configFunct.position else 1
                        position_index = configFunct.position[0] if configFunct.position else 0
                        tvals = configFunct.value.getValues() 
                        tval =  tvals[0] if ( len( tvals ) > 0 ) else 0.0
                        sliderLabel = configFunct.sliderLabels[0] if ( len(configFunct.sliderLabels) > 0 ) else ''
                        bbar.commandeerControl( position_index, sliderLabel, configFunct.getSliderBounds(), tval  )
                        bbar.positionSlider( position_index, n_active_sliders )
                        self.current_configuration_mode = configFunct.label
    #                    print " ButtonBarWidget: restore current_configuration_mode = ", configFunct.label

class ButtonBar:
    
    def __init__( self, name, interactor, **args ):
        self.name = name
        self.interactor = interactor
        self.buttons = []
        self.updateWindowSize()
        self.visible = False
        self.position = args.get( 'position', ( 0.0, 1.0 ) )
        self.vtk_coord = vtk.vtkCoordinate()
        self.vtk_coord.SetCoordinateSystemToNormalizedDisplay()
        self.StateChangedSignal = SIGNAL('StateChanged')
        self.process_mode = ProcessMode.Default
        self.origin = args.get( 'origin', OriginPosition.Upper_Left )
        self.orientation = args.get( 'orientation', Orientation.Vertical )
#        print " ButtonBar[%s]: %s" % ( name, str(self.position) )
        self.buffer = args.get( 'buffer', ( 3, 3 ) )
        self.fullButtonWindowSize = 1300
        self.magnification = args.get( 'mag', 1.0 )

    def getButton(self, name ): 
        for b in self.buttons:
            if b.id == name: return b
        return None

    def getActiveButton(self): 
        for b in self.buttons:
            if b.getState(): return b
        return None

    def updateWindowSize(self):
        self.windowSize = self.interactor.GetRenderWindow().GetSize() if ( self.interactor <> None ) else [ 100, 100 ]

    def placeButton( self, button, position, **args ):
        max_size = button.size()
        window_size = min( self.windowSize[0], self.windowSize[1] ) 
        scale = float(window_size) * self.magnification / self.fullButtonWindowSize
        if scale > 1.0:   scale = 1.0
        if scale < 0.5:   scale = 0.5
#        print " ################################# Resize Button %s: ws=%d, scale=%s, pos=%s " % ( button.id, window_size, str(scale), str(position) )
        size = [ max_size[0]*scale, max_size[1]*scale ]
        bounds = self.computeBounds( position, size )
#        print " placeButton[%s]: bounds = %s" % ( button.id, str(bounds) )
        button.place( bounds )
        return self.getOffsetScreenPosition( size, position )
    
    def render( self ):
        self.interactor.GetRenderWindow().Render()

    def build( self, **args ):
        self.current_location = self.getScreenPosition( self.position, **args )
        for button in self.buttons:
            self.current_location = self.placeButton( button, self.current_location )
            
    def reposition( self, **args ):
        self.updateWindowSize()
#        print "Reposition: %d " % self.windowSize[0]
        self.build( **args )
           
    def getScreenPosition(self, normalized_display_position, buffered = True, **args ):
        self.vtk_coord.SetValue(  normalized_display_position[0], normalized_display_position[1] )
        ren = self.getRenderer()
        screen_pos = self.vtk_coord.GetComputedDisplayValue( ren ) if ( ren <> None ) else [ 100, 100 ]
        position_offset = args.get( 'offset', [ 0, 0 ] )
        if   self.orientation == Orientation.Vertical: position_offset[ 0 ] = 0
        elif self.orientation == Orientation.Horizontal: position_offset[ 1 ] = 0
        if buffered: screen_pos = self.getBufferedPos( screen_pos, position_offset  )
#        print " GetScreenPosition [",  self.name, "], position = ", str( normalized_display_position ), "], screen position = ", str( screen_pos )
        return screen_pos
  
    def getBufferedPos( self, screen_pos, position_offset = [ 0, 0 ] ): 
        buff_screen_pos = list( screen_pos )         
        for ic in range(2):
            if self.origin[ic]:  buff_screen_pos[ic] = screen_pos[ic] - self.buffer[ic] - position_offset[ic]
            else:                buff_screen_pos[ic] = screen_pos[ic] + self.buffer[ic] + position_offset[ic]
        return buff_screen_pos
            
    def getOffsetScreenPosition( self, bsize, current_location ):
        offset_location = list( current_location )
        offset = [ bsize[0] + self.buffer[0], bsize[1] + self.buffer[1] ]
        if self.orientation == Orientation.Vertical:
            offset_location[1] = offset_location[1] - offset[1] if self.origin[1] else offset_location[1] + offset[1]
        if self.orientation == Orientation.Horizontal:
            offset_location[0] = offset_location[0] - offset[0] if self.origin[0] else offset_location[0] + offset[0]
        return offset_location

    def clear( self, **args ):
        current_button_id = args.get( 'current', None )
        for b in self.buttons:
            if ( current_button_id == None ) or ( b.id <> current_button_id ):
                if b.getState() <> 0:
                    b.setButtonState( 0 )

    def computeBounds( self, pos, size ):
        bds = [0.0]*6
        bds[0] = pos[0] - size[0] if self.origin[0] else pos[0]
        bds[1] = pos[0] if self.origin[0] else pos[0] + size[0]
        bds[2] = pos[1] - size[1] if self.origin[1] else pos[1]
        bds[3] = pos[1] if self.origin[1] else pos[1] + size[1]
        return bds
    
    def show( self, **args ):
        self.visible = True
        for button in self.buttons: button.On()

    def setVisibility( self, isVisible ):
        for button in self.buttons:
            if isVisible:
                button.buttonRepresentation.SetVisibility(1)
                if button.getState(): button.setState(1)
            else:
                button.buttonRepresentation.SetVisibility(0)

    def processKeyEvent( self, key, ctrl = 0 ):
        processed = False
        for button in self.buttons: 
            if button.processKeyEvent( key, ctrl ): 
                processed = True
        return processed
 
    def hide(self):
        self.visible = False
        for button in self.buttons:
            button.Off()
            
    def toggleVisibility(self):
        if self.visible: 
            self.hide()
        else:
            self.updatePositions() 
            self.show()
            
    def reset(self, active_state=None ):
        pass

    def getRenderer(self):
        if self.interactor == None: return None
        rw = self.interactor.GetRenderWindow()
        return rw.GetRenderers().GetFirstRenderer ()
                
class ControlBar(ButtonBar):

    def __init__( self, name, interactor, **args ):
        ButtonBar.__init__( self, name, interactor, **args )

    @classmethod
    def create( cls, name, interactor, build_args ):
        cbar = ControlBar( name, interactor, position=( 0.5, 0.0 ) )
        cbar.init( build_args )
        return cbar
        
    def init( self, build_args, **args ):   # ( "Step", ("Run","Stop") ), self.processAnimationControl
        button_specs = build_args[0]
        self.processStateChangeEvent = build_args[1]
        for bspec in button_specs:
            if ( len( bspec ) == 2 ) and isinstance( bspec[1], bool ):
                self.addButton( bspec[0], toggle=bspec[1], **args ) 
            else: 
                self.addButton( bspec, **args )

    def changeButtonActivations(self, activation_list ):
#        print " ** Change Button Activations: ", str( activation_list )
        for activation_spec in activation_list:
            self.changeButtonActivation( *activation_spec )

    def changeButtonActivation(self, button_name, activate, state = None ):
        button = self.getButton( button_name )
#        print " ---> change Button Activation[%s], activate = %s, state = %s" % ( button_name, str(activate), str(state) )
        if button:
            if activate: button.activate()
            else: button.deactivate()
        if state <> None:
            button.setToggleState( state )

    def addButton( self, bspec, **args ):
        if hasattr(bspec, "__iter__"):
            bnames = bspec
        else: bnames = [ bspec ] 
        toggle = args.get( 'toggle', False )       
        button = Button( self.interactor, names=bnames, toggle = toggle )
        button.PublicStateChangedSignal.connect( self.processStateChangeEvent )
        self.buttons.append( button )

    def reset( self, active_state=None  ):
        if ( active_state == None ) or ( self.name <> active_state ):
            self.hide()
  
class ButtonBarWidget(ButtonBar):
        
    def __init__( self, handler, name, interactor, **args ):
        ButtonBar.__init__( self, name, interactor, **args )
        self.handler = handler
        self.currentControls = {}
        self.slider_postions = [ [ [ 0.25, 0.75 ] ], [ [0.01,0.48], [0.52, 0.99 ] ], [ [0.01,0.3], [0.35,0.7], [0.75, 0.99 ] ], [ [0.01,0.24], [0.26,0.49], [0.51,0.74], [0.76, 0.99 ] ]    ]
        self._slidersVisible = [ False, False, False, False ]
        self.InteractionState = None
        self.LastInteractionState = None
        self.activeSliceIndex = 0  
        self.groups = {}
        self.configurableFunctions = collections.OrderedDict()
        self.interaction_priority = 0

    def setVisibility( self, isVisible ):
        ButtonBar.setVisibility( self, isVisible )
        for ( process_mode, interaction_state, swidget ) in self.currentControls.values():
            if not isVisible: swidget.GetRepresentation().VisibilityOff()
            else:             swidget.GetRepresentation().VisibilityOn()

    def show( self, **args ):
        self.initializeChildren( **args )
        ButtonBar.show( self, **args )
        
    def isSliderVisible( self, islider ):
        return self._slidersVisible[ islider ]

    def setSliderVisibility( self, islider, isVisible ):
        self._slidersVisible[ islider ] = isVisible
#         if islider == 2: 
#             print " setSliderVisibility[%d] = %s " % ( islider, str(isVisible))
                            
    def initializeState(self):
        for ib in self.buttons:
#            print "Initialize Button '%s': %s " % ( ib.id, str(ib.getState()) )
            if ib.getState() > 0:
                ib.refreshButtonState()
                self.processStateChangeEvent( ib.id, ib.key, ib.getState(), True )

     
                   
    def sliceRoundRobin(self, args, config_function = None ):
        if args[0] == "InitConfig":
            self.activeSliceIndex = ( self.activeSliceIndex+ 1 ) % 3
            toggle_list = self.groups.get( config_function.name, [] )
            for iSlice in range( len(toggle_list) ):
                button = toggle_list[ iSlice ]
                state = 1 if (iSlice == self.activeSliceIndex) else 0
                button.setButtonState( state )
                
       

    def resetInteractionButtons( self, current_button, new_state ):
        ibbar = self.handler.getButtonBar( 'Interaction' )
#        print " resetInteractionButtons: ", str( [ current_button, new_state ] )
        for ib in ibbar.buttons:
            is_child = ib.id in current_button.children
            state = new_state if is_child else 0
#           ibbar.processStateChangeEvent( ib.id, ib.key, state )
            if state <> ib.getState():
                ib.setButtonState(state)           
                if is_child:
                    if ( new_state == 0 ): ib.deactivate()
                    else: ib.activate()
                    
    def processStateChangeEvent( self, button_id, key, state, force = False ):
        b = self.getButton( button_id )
        if (b.getState() <> state) or (not b.toggle) or force:
#            print " processStateChangeEvent: ", str( [ button_id, key, state ] )
            b.setState(state)
            if state > 0: 
                self.updateInteractionState( button_id, state  ) 
            else:
                if not b.toggle: 
                    self.updateInteractionState( button_id, state  )                
                else:
                    if self.name == "Plot": self.resetInteractionButtons( b, 0 )
                    else: self.InteractionState = None
                    configFunct = self.configurableFunctions.get( button_id, None )
                    position_index = configFunct.getPosition() if configFunct else None
                    positions = [ position_index ] if position_index else range(4)
                    for pindex in positions: self.releaseSlider( pindex ) 
                    configFunct.processInteractionEvent( [ "InitConfig", 0, False, self ] )
                    if (self.name <> "Configure"): self.handler.restoreInteractionState( state )

            self.StateChangedSignal( button_id, key, state )

    #        config_function = self.configurableFunctions.get( button_id, None )
    #        if config_function: config_function.processStateChangeEvent( state )
    #        button = self.buttons.get( button_id, None )
    


    def addSliderButton( self, **args ):
        button = self.addButton( **args )
        cf = self.addConfigurableSliderFunction( button.id, **args)
        button.PrivateStateChangedSignal.connect( cf.setState )
        cf_state = cf.getState()
        if cf_state <> None: 
            button.setState( cf_state )
        return button
        
    def addConfigButton( self, **args ):
        button = self.addButton( **args )
        cf = self.addConfigurableFunction( button.id, **args)
        button.PrivateStateChangedSignal.connect( cf.setState )
        cf_state = cf.getState()
        if cf_state <> None: 
            button.setState( cf_state )
        return button

    def addButton( self, **args ):
        button = Button( self.interactor, **args )
        button.PublicStateChangedSignal.connect( self.processStateChangeEvent )
        self.buttons.append( button )
#        print " ButtonBarWidget[%x]: addButton[%x] %s " % ( id(self), id(button), button.id )
        roundRobin = args.get( 'group', None )
        if roundRobin:
            grpList = self.groups.setdefault(roundRobin,[])
            grpList.append( button )
        return button
            
    def addConfigurableFunction(self, name, **args):
        cf = self.handler.cfgManager.getConfigurableFunction( name, **args )
        self.configurableFunctions[name] = cf
        return cf

    def addConfigurableSliderFunction(self, name, **args):
        cf = self.handler.cfgManager.getConfigurableFunction( name, type = ConfigurableFunction.Slider, **args )
        self.configurableFunctions[name] = cf
        return cf

    def getConfigFunction( self, name ):
        return self.configurableFunctions.get(name,None)

    def getConfigFunctions( self ):
        return self.configurableFunctions.values()

    def removeConfigurableFunction(self, name ):        
        del self.configurableFunctions[name]

    def applyConfiguration(self, **args ):       
        for configFunct in self.configurableFunctions.values():
            configFunct.applyParameter( **args  )

    def updateSliderWidgets(self, values ):
        for index, value in enumerate( values ):
            if value <> None: self.setSliderValue( index, value )
        self.render()

    def setSliderValue(self, index, value ):
        ( process_mode, interaction_state, swidget ) = self.currentControls.get( index, ( None, None, None ) )
        if swidget:
            srep = swidget.GetRepresentation( )
            self.setInteractionPriority( 1 )
            swidget.InvokeEvent("StartInteractionEvent")
            srep.SetValue( value  )
            swidget.InvokeEvent("InteractionEvent")
            swidget.InvokeEvent("EndInteractionEvent")

    def setInteractionPriority( self, priority ):
        self.interaction_priority = priority
            
    def initializeSliderPosition( self, index ):
        try:
            ( process_mode, interaction_state, swidget ) = self.currentControls.get( index, ( None, None, None ) )
            if swidget:
                srep = swidget.GetRepresentation( ) 
                values = self.handler.cfgManager.getParameterValue( interaction_state )
                value = ( ( srep.GetMinimumValue() + srep.GetMaximumValue() ) / 2.0 )  if ( values == None ) else values[0]
                srep.SetValue( value  ) 
        except Exception, err:
            print str(err)
                    
    def createSliderWidget( self, index ): 
        sliderRep = vtk.vtkSliderRepresentation2D()
            
        sliderRep.GetPoint1Coordinate().SetCoordinateSystemToNormalizedDisplay()
        sliderRep.GetPoint2Coordinate().SetCoordinateSystemToNormalizedDisplay()
        prop = sliderRep.GetSliderProperty()
        prop.SetColor( 1.0, 0.0, 0.0 )
        prop.SetOpacity( 0.5 )
        sprop = sliderRep.GetSelectedProperty()
        sprop.SetOpacity( 0.8 )           
        tprop = sliderRep.GetTubeProperty()
        tprop.SetColor( 0.5, 0.5, 0.5 )
        tprop.SetOpacity( 0.5 )
        cprop = sliderRep.GetCapProperty()
        cprop.SetColor( 0.0, 0.0, 1.0 )
        cprop.SetOpacity( 0.5 )
#        sliderRep.PlaceWidget(  bounds   )  
        sliderRep.SetSliderLength(0.05)
        sliderRep.SetSliderWidth(0.02)
        sliderRep.SetTubeWidth(0.01)
        sliderRep.SetEndCapLength(0.02)
        sliderRep.SetEndCapWidth(0.02)
        sliderRep.SetTitleHeight( 0.02 )    
        sliderWidget = vtk.vtkSliderWidget()
        sliderWidget.SetInteractor(self.interactor)
        sliderWidget.SetRepresentation( sliderRep )
        sliderWidget.SetAnimationModeToAnimate()
        sliderWidget.EnabledOn()
        sliderWidget.AddObserver("StartInteractionEvent", self.processStartInteractionEvent )
        sliderWidget.AddObserver("EndInteractionEvent", self.processEndInteractionEvent )
        sliderWidget.AddObserver("InteractionEvent", self.processInteractionEvent )

#        sliderWidget.AddObserver("AnyEvent", self.processSliderEvent )
#        eventTranslator = sliderWidget.GetEventTranslator()
#        eventTranslator.SetTranslation (vtk.vtkCommand.RightButtonPressEvent, vtk.vtkWidgetEvent.ModifyEvent )
        sliderWidget.KeyPressActivationOff()
        return sliderWidget

    def processSliderEvent(self, *args ):
        print '-----> processSliderEvent: ', str( args )
    
    def positionSliders( self, nsliders ): 
        for islider in range( nsliders ):
            self.positionSlider( islider, nsliders )
            
    def releaseSliders( self ):
        for index in range(4): 
            self.releaseSlider( index )            
            
    def positionSlider(self, position_index, n_sliders ):
        slider_pos = self.slider_postions[ n_sliders ]
        ( process_mode, interaction_state, swidget ) = self.currentControls[position_index]
        sliderRep = swidget.GetRepresentation( ) 
        sliderRep.GetPoint1Coordinate().SetValue( slider_pos[position_index][0], 0.06, 0 )  
        sliderRep.GetPoint2Coordinate().SetValue( slider_pos[position_index][1], 0.06, 0 )
        sliderRep.Modified()
        swidget.Modified()    
        sliderRep.NeedToRenderOn()

    def setSliderValues( self, values ):
        for index, value in enumerate(values):
            widget_item = self.currentControls.get( index, None )
            if widget_item == None: 
                swidget = self.createSliderWidget(index)
                self.currentControls[index] = ( self.process_mode, self.InteractionState, swidget ) 
            else:
                ( process_mode, interaction_state, swidget ) = widget_item
            srep = swidget.GetRepresentation( ) 
            try: srep.SetValue( value )  
            except TypeError:
                print>>sys.stderr, "Type Error setting slider-%d value: " % index, str( value )
                        
    def commandeerControl(self, index, label, bounds, tvals ): 
        if bounds == None: return
#        print " CommandeerSlider[%d]: ('%s') %s: %s in %s " % ( index, label, self.InteractionState, str(tvals), str(bounds) )
        widget_item = self.currentControls.get( index, None ) 
        isButtonWidget = type(label) == list
        if widget_item == None: 
            if isButtonWidget:
                swidget = self.createButtonWidget( index, label ) 
            else:
                swidget = self.createSliderWidget(index) 
        else:
            ( process_mode, interaction_state, swidget ) = widget_item
        
        if isButtonWidget:
            pass
        else:   
            value = get_scalar_value( tvals )
            srep = swidget.GetRepresentation( )      
            srep.SetTitleText( label )
            srep.SetMinimumValue( bounds[ 0 ] )
            srep.SetMaximumValue( bounds[ 1 ]  )
            srep.SetValue( value )
            swidget.SetEnabled( 1 )         
        self.currentControls[index] = ( self.process_mode, self.InteractionState, swidget )
        
        
    def createButtonWidget(self, index, label ):
        pass
    
    def releaseSlider( self, index ):      
        ( process_mode, interaction_state, swidget ) = self.currentControls.get( index, ( None, None, None ) )  
        if swidget: 
            swidget.SetEnabled( 0 ) 
#            print "Releasing slider[%d]: %s " % ( index, interaction_state )

    def getSliderEnabled( self, index ):        
        ( process_mode, interaction_state, swidget ) = self.currentControls.get( index, ( None, None, None ) )  
        if swidget: return swidget.GetEnabled() 
        return False
        
    def clearInteractions(self):
        if self.InteractionState <> None: 
            configFunct = self.configurableFunctions[ self.InteractionState ]
            configFunct.close()   
        self.process_mode = ProcessMode.Default
        self.InteractionState = None
        for ( process_mode, interaction_state, swidget ) in self.currentControls.values():
            swidget.SetEnabled( 0 ) 
        self.render()

    def processInteractionEvent( self, obj=None, event=[] ):
        if ( self.InteractionState <> None ): 
            srep = obj.GetRepresentation( ) 
            config_function = self.getConfigFunction( self.InteractionState )
            if config_function <> None:
                config_function.processInteractionEvent( [ "UpdateConfig", self.getSliderIndex(obj), srep, event, self.interaction_priority  ] )
                self.interaction_priority = 0
            else:
                print>>sys.stderr, " FAILED processInteractionEvent[%s]: ( %s %d )" % ( self.name, self.InteractionState, self.process_mode )
                                       
#         else:
#             if self.process_mode == ProcessMode.Slicing:
#                 ( process_mode, interaction_state, swidget ) = self.currentControls[1] 
#                 slice_pos = swidget.GetRepresentation( ).GetValue()
#                 self.pushSlice( slice_pos )         

    def processStartInteractionEvent( self, obj, event ): 
        slider_index = self.checkInteractionState( obj, event ) 
#        print " processStartInteractionEvent: ( %s %d %d )" % ( self.InteractionState, self.process_mode, shift_key )
        if ( self.InteractionState <> None ): 
            config_function = self.getConfigFunction( self.InteractionState )
            if config_function <> None:
                shift_key = self.interactor.GetShiftKey()
                if shift_key: self.processWidgetShiftCommand(  obj, slider_index, config_function )
                config_function.processInteractionEvent( [ "StartConfig", slider_index, self.interaction_priority ] )
            else:
                print>>sys.stderr, " FAILED processStartInteractionEvent[%s]: ( %s %d )" % ( self.name, self.InteractionState, self.process_mode )
    
    def processWidgetShiftCommand(self, obj, slider_index, config_function  ): 
        pass
#        print " --------->>>>> processWidgetToggleCommand:  %s %d %s" % ( str(obj), slider_index, str(config_function)  )  
                
    def checkInteractionState( self, obj, event ):
        for item in self.currentControls.items():
            ( process_mode, interaction_state, swidget ) = item[1]
            if ( id( swidget ) == id( obj ) ): 
                if self.InteractionState <> interaction_state:            
                    self.processEndInteractionEvent( obj, event )
                    if self.InteractionState <> None: self.endInteraction()
                    self.InteractionState = interaction_state
                    self.process_mode = process_mode
                    #print " --> Change Interaction State: %s %d " % ( self.InteractionState, self.process_mode )
                return item[0]
        return None
            
    def getSliderIndex(self, obj ):
        for index in self.currentControls:
            ( process_mode, interaction_state, swidget ) = self.currentControls[index]
            if ( id( swidget ) == id( obj ) ): return index
        return None

    def processEndInteractionEvent( self, obj, event ):  
#        print " processEndInteractionEvent: ( %s %d )" % ( self.InteractionState, self.process_mode )
        if ( self.InteractionState <> None ): 
            config_function = self.getConfigFunction( self.InteractionState )
            if config_function <> None:
                config_function.processInteractionEvent( [ "EndConfig" ] )
            else:
                print>>sys.stderr, " FAILED processEndInteractionEvent[%s]: ( %s %d )" % ( self.name, self.InteractionState, self.process_mode )

    def startConfiguration( self, x, y, config_types ): 
        if (self.InteractionState <> None) and not self.configuring:
            configFunct = self.configurableFunctions[ self.InteractionState ]
            if configFunct.type in config_types:
                self.configuring = True
                configFunct.start( self.InteractionState, x, y )
                self.haltNavigationInteraction()
#                if (configFunct.type == 'leveling'): self.getLabelActor().VisibilityOn()

    def haltNavigationInteraction(self):
        if self.interactor: 
            istyle = self.interactor.GetInteractorStyle () 
            istyle.Off()  
    
    def resetNavigation(self):
#        print " ----------------------BBW resetNavigation -------------------------- "
        if self.interactor:
            istyle = self.interactor.GetInteractorStyle () 
            istyle.On()  
            self.enableVisualizationInteraction()

    def getInteractionState( self, key ):
        for configFunct in self.configurableFunctions.values():
            if configFunct.matches( key ): return ( configFunct.name, configFunct.persisted, self )
        return ( None, None, None )  
    
    def reset(self, active_state=None ):
        self.releaseSliders()
    
    def updateInteractionState( self, config_state, button_state, **args ): 
#         rcf = None
#         if config_state == None: 
#             self.finalizeLeveling()
#             self.endInteraction()   
#         else:
#        print " Update Interaction State: %s %s %s " % ( str(config_state), str(button_state), str(args) )
        configFunct = self.configurableFunctions.get( config_state, None )
        if self.InteractionState <> None: 
            prevConfigFunct = self.configurableFunctions[ self.InteractionState ]
            sameGroup = prevConfigFunct.sameGroup( configFunct )
            if not sameGroup: prevConfigFunct.close()  
        else: sameGroup = False
        
        if configFunct:
#            child_activations = []
#                if configFunct.type <> 'slider': 
#                    self.releaseSliders() 
            configFunct.open( config_state )
            self.InteractionState = config_state  
            #print " ---> UpdateInteractionState: %s " % str(self.InteractionState)                  
            if button_state: 
                self.LastInteractionState = self.InteractionState
#            self.disableVisualizationInteraction()
            initialize_config_state = ( configFunct.label <> self.handler.current_configuration_mode )               
            configFunct.processInteractionEvent( [ "InitConfig", button_state, initialize_config_state, self ] )
            if initialize_config_state:
                bbar = self.handler.getButtonBar( 'Interaction' )
                active_button = configFunct.name if ( self.name == bbar.name ) else None
                child_activations = bbar.initConfigState( active_button=active_button )
                if self.name == "Plot": self.resetInteractionButtons( self.getButton( config_state ), 1 )    
            
            if (configFunct.type == 'slider'):
                force_enable = args.get( 'enable', False )

                tvals = configFunct.value.getValues()
                if not sameGroup: 
                    for bbar in self.handler.getButtonBars():
                        bbar.reset( config_state )                
                if configFunct.position <> None:
                    n_active_sliders = configFunct.position[1]
                    position_index = configFunct.position[0]
                    
                    if initialize_config_state:
                        for iSlice in range(4): self.setSliderVisibility( iSlice, iSlice == position_index ) 
                    else:
                        self.setSliderVisibility( position_index, button_state )
                        
#                        slicePosition = configFunct.value
#                        self.setSliderValue( position_index, slicePosition.getValue() )  
                                           
                    if self.isSliderVisible( position_index ) or force_enable:
                        tval = tvals[0]  if len( tvals ) > 0 else 0.0
                        self.InteractionState = configFunct.cfg_state
                        self.commandeerControl( position_index, configFunct.sliderLabels[0], configFunct.getSliderBounds(), tval  )
                        self.positionSlider( position_index, n_active_sliders )
                        self.setSliderVisibility( position_index, True )
                    else: self.releaseSlider( position_index )
                else:
                    n_active_sliders = len( configFunct.sliderLabels )
                    for slider_index in range(4): self.setSliderVisibility( slider_index, slider_index < n_active_sliders )
                    for slider_index in range(4):
                        if self.isSliderVisible( slider_index ) and ( len(tvals) > slider_index ):
                            self.commandeerControl( slider_index, configFunct.sliderLabels[slider_index], configFunct.getSliderBounds(), tvals[slider_index]  )
                            self.positionSlider( slider_index, n_active_sliders )
                        else:
                            self.releaseSlider( slider_index )
                self.handler.current_configuration_mode = configFunct.label
#                print " self.handler.current_configuration_mode = ", configFunct.label
                configFunct.processInteractionEvent( [ "ProcessSliderInit" ] )
#                for child_button in child_activations: 
#                    child_button.setButtonState(1)
            self.render()                     
#         elif config_state == 'reset':
#             self.resetCamera()              
#             if  len(self.persistedParameters):
#                 pname = self.persistedParameters.pop()
#                 configFunct = self.configurableFunctions[pname]
#                 param_value = configFunct.reset() 
#                 if param_value: self.persistParameterList( [ (configFunct.name, param_value), ], update=True, list=False )                                      
        return configFunct

    def enableVisualizationInteraction(self): 
        pass

    def disableVisualizationInteraction(self):
        pass
    
    def printInteractionStyle(self, msg ):
        print "%s: InteractionStyle = %s " % ( msg,  self.renderWindow.GetInteractor().GetInteractorStyle().__class__.__name__ ) 

    def finalizeConfigurationObserver( self, parameter_name, **args ):
        self.finalizeParameter( parameter_name, **args )
        self.endConfiguration()    
#        for parameter_name in self.getModuleParameters(): self.finalizeParameter( parameter_name, *args ) 
        self.endInteraction( **args ) 

    def finalizeParameter(self, parameter_name, **args ):
        pass
    
    def endInteraction( self, **args ):  
        self.resetNavigation() 
        self.configuring = False
        self.InteractionState = None
        self.enableVisualizationInteraction()

    def endConfiguration( self ):
        pass
    
    def updateChildState(self, child_button, parent_name ):
        if len( child_button.parents ):
            if ( parent_name in child_button.parents ):
                parent_button = self.handler.findButton( parent_name )
                if (parent_button <> None) and parent_button.getState(): 
                    if self.visible: 
                        child_button.activate() 
#                        print "Activate child: ", child_button.id
                    if child_button.id in parent_button.children:
                        return True
                else:                       
                    child_button.deactivate()
#                    print "Dectivate child: ", child_button.id
        return False
                
    
    def initConfigState( self, **args ):
        active_button = args.get( 'active_button', '' )
        child_activations = [ ]
        for button in self.buttons:
            if (active_button == None) and (button.getState() > 0):
                active_button = button.id 
            if self.updateChildState( button, active_button ):
                child_activations.append( button )
            else:
                if button.toggle and (button.id <> active_button):
                    button.setToggleState(0)
        return child_activations
           
    def initializeConfiguration( self, **args ):
        for configFunct in self.configurableFunctions.values():
            configFunct.init( **args )
#             try: configFunct.init( **args )
#             except Exception, err:
#                 print>>sys.stderr, "Error initializing configurableFunction %s: %s" % ( configFunct.name, str(err)  )
        for button in self.buttons:
            if button.toggle:
                button.broadcastState( button.getState() )
                
    def initializeChildren( self, **args ):
        for button in self.buttons:
            for parent_name in button.parents:
                self.updateChildState( button, parent_name )

def processTestStateChange( button_id, key, state, force = False ):
    print " processTestStateChange: state  = %d "% state
