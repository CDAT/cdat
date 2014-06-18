'''
Created on May 13, 2014

@author: tpmaxwel
'''
import cdms2
from PointCloudViewer import CPCPlot
#from VolumeViewer import VolumePlot
from SliceViewer import SlicePlot
from RectilinearGridPlot import RectGridPlot
from ConfigurationFunctions import PlotType

class DV3D:
    
    def __init__( self, **args ):
        self.plot = None
    
    
    def init(self, **args ):
        
        init_args = args[ 'init' ]
        ( grid_file, data_file, interface, varnames, grd_coords, var_proc_op, ROI, subSpace ) = init_args
        df = cdms2.open( data_file )       
        var = df[ varnames[0] ]
        grid_metadata = var.getGrid()

        plot_type = args.get( 'plot_type', PlotType.getPointsLayout( grid_metadata ) )
        
        if plot_type == PlotType.Grid:
            self.plot = RectGridPlot(**args) 
            self.plot.init( **args ) 
        else:
            self.plot = CPCPlot(**args) 
            self.plot.init( **args  ) 
            
    def getRenderWindow(self):
        return self.plot.renderWindow 
    
    def onKeyEvent( self, eventArgs ): 
        return self.plot.onKeyEvent( eventArgs )               

    def terminate( self ): 
        return self.plot.terminate( )               
        
