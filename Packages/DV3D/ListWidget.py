'''
Created on May 22, 2014

@author: tpmaxwel
'''
import vtk
import numpy as np
from ColorMapManager import *
from ConfigurationFunctions import SIGNAL

def getScalars( image_data, x, y ):
    comp_data = [ int( image_data.GetScalarComponentAsFloat ( x, y, 0, ic ) ) for ic in range( 4 ) ]
    return str( comp_data )
        
class ListWidget:
    
    def __init__( self, interactor, **args ):
        self.StateChangedSignal = SIGNAL('StateChanged')
        self.buttonRepresentation = None
        self.interactor = interactor
        self.buttons = {}
        self.visible = False
        self.windowSize = self.interactor.GetRenderWindow().GetSize()
        
    def processStateChangeEvent( self, button, event ):
        button_rep = button.GetSliderRepresentation()
        state = button_rep.GetState()
        button_specs = self.buttons[ button ]
        button_id = button_specs[ 0 ]
        self.StateChangedSignal( button, [ button_id, state ] )
            
    def getButton( self, **args ):
        button_id, buttonRepresentation = self.getButtonRepresentation( **args )
        buttonRepresentation.SetPlaceFactor( args.get( 'scale', 1 ) )
        position = args.get( 'position', [ 1.0, 1.0 ] )
        size = args.get( 'size', [ 100.0, 20.0 ] )
        buttonRepresentation.PlaceWidget( self.computeBounds(position,size) )
        buttonWidget = vtk.vtkButtonWidget()
        buttonWidget.SetInteractor(self.interactor)
        buttonWidget.SetRepresentation(buttonRepresentation)
        buttonWidget.AddObserver( 'StateChangedEvent', self.processStateChangeEvent )
        self.buttons[ buttonWidget ] = [ button_id, position, size ]
        return buttonWidget

    def checkWindowSizeChange( self ):
        new_window_size = self.interactor.GetRenderWindow().GetSize()
        if ( self.windowSize[0] <> new_window_size[0] ) or ( self.windowSize[1] <> new_window_size[1] ):
            self.windowSize = new_window_size
            return True
        else: 
            return False
        
    def updatePositions(self): 
        if self.checkWindowSizeChange():
            for button_item in self.buttons.items():
                button = button_item[0]
                [ button_id, position, size ] = button_item[1]
                brep = button.GetRepresentation()
                brep.PlaceWidget( self.computeBounds(position,size) )
                brep.Modified()
                button.Modified()
    
    def build(self):
        pass
    
    def getButtonRepresentation(self):
        return None, None
    
    def show(self):
        self.visible = True
        for button in self.buttons.keys():
            button.On()
#            button.Render()
 
    def hide(self):
        self.visible = False
        for button in self.buttons.keys():
            button.Off()
            
    def toggleVisibility( self, **args ):
        state = args.get( 'state', None )
        if state <> None: self.visible = True if ( state == 0 ) else False  
        if self.visible: 
            self.hide()
        else:
            self.updatePositions() 
            self.show()
            
    def getRenderer(self):
        rw = self.interactor.GetRenderWindow()
        return rw.GetRenderers().GetFirstRenderer ()
            
    def computeBounds( self, normalized_display_position, size ):
        renderer = self.getRenderer()
        upperRight = vtk.vtkCoordinate()
        upperRight.SetCoordinateSystemToNormalizedDisplay()
        upperRight.SetValue( normalized_display_position[0], normalized_display_position[1] )
        bds = [0.0]*6
        bds[0] = upperRight.GetComputedDisplayValue(renderer)[0] - size[0]
        bds[1] = bds[0] + size[0]
        bds[2] = upperRight.GetComputedDisplayValue(renderer)[1] - size[1]
        bds[3] = bds[2] + size[1]
        return bds

# class TextListWidget(ListWidget):    
# 
#     def __init__( self, interactor, **args ):
#         ListWidget.__init__( self, interactor, **args )   
# 
#     def getButtonRepresentation(self, **args):
#         labels = args.get( 'labels', [] )
#         buttonRepresentation = vtk.vtkProp3DButtonRepresentation()
#         nstates = len( labels )
#         buttonRepresentation.SetNumberOfStates(nstates)
#         for button_index in range( nstates ):
#             text_actor = vtk.vtkTextActor()
#             text_actor.SetInput(labels[button_index])
#             text_actor.GetTextProperty().SetColor((1, 1, 1))
#             buttonRepresentation.SetButtonProp( button_index, text_actor )
#         return buttonRepresentation
               
class ColorbarListWidget(ListWidget):    

    def __init__( self, interactor, **args ):
        ListWidget.__init__( self, interactor, **args )     
        self.lut = vtk.vtkLookupTable()
        self.image_data = {}
        self.colorMapManager = ColorMapManager( self.lut ) 
        self.textMapper = None
        self.build()
        
    def build(self):
        cmap_names = self.colorMapManager.getColormapNames()
        dy = 1.0 / len( cmap_names )
        for cmap_index, cmap_name in enumerate( cmap_names ):
            self.getButton( name=cmap_name, position = ( 1.0, 1.0 - dy * cmap_index )  )
                             
    def getButtonRepresentation(self, **args):
        buttonRepresentation = vtk.vtkTexturedButtonRepresentation2D()
        cmap_name = args.get( 'name', None )
        button_id = args.get( 'id', cmap_name )
        if cmap_name:
            buttonRepresentation.SetNumberOfStates(2)
            for image_index in range( 2 ):
                image_data = self.getColorbarImage( cmap_name, invert=(image_index==1) )
                buttonRepresentation.SetButtonTexture( image_index, image_data )
#         labels = args.get( 'labels', None )
#         if labels:
#             if self.textMapper == None:
#                 size = args.get( 'size', [ 30.0, 30.0 ] )
#                 self.textMapper = vtk.vtkFreeTypeUtilities()
#             nstates = len( labels )
#             buttonRepresentation.SetNumberOfStates(nstates)
#             tprop = vtk.vtkTextProperty()
#             for label_index in range( nstates ):
#                 texture = vtk.vtkImageData()
#                 label = labels[label_index]
#                 if button_id == None: button_id = label
#                 self.textRenderer.RenderString( tprop, label, 0, 0, texture ) 
#                 buttonRepresentation.SetButtonTexture( label_index, texture )
        return button_id, buttonRepresentation

    def getColorbarImage(self, cmap_name, **args ):
        cb_width = args.get( 'width', 40 )
        invert = args.get('invert',False)
        cmap_data = self.colorMapManager.load_array( cmap_name ) * 255.9 
        if invert: cmap_data = cmap_data[::-1,:]
        cmap_data = np.expand_dims( cmap_data[:,0:3].astype('uint8'), 0 )
        cmap_data = np.tile( cmap_data, ( cb_width, 1, 1 ) )       
        self.image_data[ cmap_name + ( ".inv" if invert else '' ) ] = cmap_data
        image = vtk.vtkImageData()
        image.SetDimensions(256,cb_width,1)
        vtkdata = vtk.vtkUnsignedCharArray() 
        vtkdata.SetNumberOfComponents( 3 )
        vtkdata.SetNumberOfTuples( cb_width * 256 )
        vtkdata.SetVoidArray( cmap_data, cmap_data.size, 1 )
        ptdata =  image.GetPointData()
        ptdata.SetScalars(vtkdata)
        return image

def listStateChanged( obj, event ): 
    print " listStateChanged: ", str( event )    
             
if __name__ == '__main__':     
    renderer = vtk.vtkRenderer()
    renderer.SetBackground(0.1, 0.2, 0.4)
     
    render_window = vtk.vtkRenderWindow()
    render_window.AddRenderer(renderer)
    render_window.SetSize( 1600, 1200 )
     
    interactor = vtk.vtkRenderWindowInteractor()
    interactor.SetRenderWindow(render_window)

    interactor.Initialize()
    render_window.Render()
    
    list_widget = ColorbarListWidget( interactor )
    list_widget.StateChangedSignal.connect( listStateChanged )
#    list_widget.getButton( labels=[ 'test', 'ok' ] )
    list_widget.show()
         
    render_window.Render()
    interactor.Start()