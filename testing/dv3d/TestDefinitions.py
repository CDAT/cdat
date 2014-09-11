'''
Created on Aug 28, 2014

@author: tpmaxwel
'''
from TestManager import TestManager, vcsTest
import vcs, sys, os

testManager = TestManager()   

test1 =  vcsTest( 'dv3d_slider_test', roi=( -105.0, -15.0, 5.0, 50.0 ), file="geos5-sample.nc", vars = [ 'uwnd' ], 
                     parameters={'VerticalScaling': 3.0,
                                 'ToggleVolumePlot': vcs.off, 
                                 'ScaleOpacity': [1.0, 1.0], 
                                 'ToggleSurfacePlot': vcs.off, 
                                 'ScaleColormap': [-10.0, 10.0, 1], 
                                 'BasemapOpacity': [0.5],
                                 'XSlider': ( -50.0, vcs.on ),
                                 'ZSlider': ( 10.0,  vcs.on ),
                                 'YSlider': ( 20.0,  vcs.on ), 
                                 }  )       
     
test2 =  vcsTest( 'dv3d_volume_test', roi=( -105.0, -15.0, 7.0, 50.0 ), file="geos5-sample.nc", vars = [ 'uwnd' ], 
                     parameters={'VerticalScaling': 3.0, 
                                 'ToggleVolumePlot': vcs.on,
                                 'ToggleSurfacePlot': vcs.off, 
                                 'ScaleOpacity': [0.0, 0.8],
                                 'ScaleColormap': [-15.0, 10.0, 1], 
                                 'ScaleTransferFunction':  [6.0, 12.0, 1], 
                                 'BasemapOpacity': [0.5],
                                 'XSlider': ( vcs.off ),
                                 'ZSlider': ( vcs.off ),
                                 'YSlider': ( vcs.off ), 
                                 }  )       


test3 =  vcsTest( 'dv3d_surface_test', roi=( -105.0, -15.0, 5.0, 50.0 ), file="geos5-sample.nc", vars = [ 'uwnd' ], 
                     parameters={'VerticalScaling': 3.0,
                                 'ToggleVolumePlot': vcs.off,  
                                 'ToggleSurfacePlot': vcs.on,
                                 'ScaleOpacity': [1.0, 1.0], 
                                 'IsosurfaceValue': [ 30.0 ],
                                 'ScaleColormap': [ 20.0, 40.0, 1], 
                                 'BasemapOpacity': [0.5],
                                 'XSlider': ( vcs.off ),
                                 'ZSlider': ( vcs.off ),
                                 'YSlider': ( vcs.off ), 
                                 }  )       
    
if __name__ == '__main__':
    
    testManager.reviewTests()
#    testManager.reviewTest( 'dv3d_slider_test' )
    
