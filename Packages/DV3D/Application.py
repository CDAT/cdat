'''
Created on May 13, 2014

@author: tpmaxwel
'''
import cdms2, sys
from PointCloudViewer import CPCPlot
#from VolumeViewer import VolumePlot
from SliceViewer import SlicePlot
from RectilinearGridPlot import RectGridPlot
from ConfigurationFunctions import PlotType

def getPlotFromVar( var, **args ):
    grid_metadata = var.getGrid()
    plot_type = args.get( 'plot_type', PlotType.getPointsLayout( grid_metadata ) )
    plot = RectGridPlot(**args) if ( plot_type == PlotType.Grid ) else CPCPlot(**args)
    return plot

class DV3DApp:
    
    def __init__( self, canvas, cell_coordinates=None, **args ):
        self.plot = None
        self.canvas = canvas
        self.cell_coordinates = cell_coordinates
    
    
#     def init(self, **args ):
#         var= None
#         init_args = args.get( 'init', None )
#         if init_args:
#             ( grid_file, data_file, interface, varnames, grd_coords, var_proc_op, ROI, subSpace ) = init_args
#             df = cdms2.open( data_file )       
#             var = df[ varnames[0] ]
#         else:
#             print>>sys.stderr, "Error, this method requires an init_args argument"
#             
#         if id(var) <> id(None):
#             self.plot = getPlotFromVar( var, **args )
#             self.plot.init( **args  ) 

    def update( self, tmpl ):
        if self.plot <> None:
            self.plot.updateModule() 
            
    def onClosing(self, cell ):
        if self.plot <> None:
            self.plot.onClosing( cell )
            
    def applyAction( self, action ):
        if self.plot <> None:
            self.plot.applyAction( action )
            
    def setAnimationStepper( self, stepper ):
        self.plot.setAnimationStepper( stepper )

    def gminit(self, var1, var2, **args ):
        grid_metadata = var1.getGrid()
        plot_type = args.get( 'plot_type', PlotType.getPointsLayout( grid_metadata ) )
        args[ 'cell_coordinates' ] = self.cell_coordinates
        
        if plot_type == PlotType.Grid:
            if self.plot == None:
                self.plot = RectGridPlot(**args) 
                self.plot.gminit( var1, var2, **args )
                self.plot.ParameterValueChanged.connect(self.canvas.processParameterChange)
            else:
                self.plot.updateModule() 
        else:
            if self.plot == None:
                self.plot = CPCPlot(**args) 
                self.plot.gminit( var1, var2, **args  ) 
                self.plot.ParameterValueChanged.connect(self.canvas.processParameterChange)
            else:
                self.plot.updateModule() 

            
    def getRenderWindow(self):
        return self.plot.renderWindow 
    
#     def onKeyEvent( self, eventArgs ): 
#         return self.plot.onKeyEvent( eventArgs )               

    def terminate( self ): 
        return self.plot.terminate( ) 
    
    def start(self): 
        iren = self.plot.renderWindow.GetInteractor() 
        iren.Start()

    def hideWidgets(self):
        self.plot.hideWidgets()

    def showWidgets(self):
        self.plot.showWidgets()

