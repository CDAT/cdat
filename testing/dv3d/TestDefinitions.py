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
                                 'ZSlider': ( 0.0,  vcs.on ),
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

test3 =  vcsTest( 'dv3d_constituents_test', file="geos5-sample.nc", vars = [ 'uwnd' ], 
                     parameters={'VerticalScaling': 5.0,
                                 'ToggleClipping': ( 40, 360, -28, 90 ),
                                 'ToggleVolumePlot': vcs.on,  
                                 'ToggleSurfacePlot': vcs.on,
                                 'ScaleOpacity': { 'Volume': [0.0, 0.3] },
                                 'IsosurfaceValue': [ 31.0 ],
                                 'ScaleColormap': ( [ -46.0, 48.0 ], { 'Volume': [ 16.0, 30.0 ], 'Surface': [ 30.0, 35.0 ] } ), 
                                 'BasemapOpacity': 0.5,
                                 'ScaleTransferFunction':   [ 12.0, 77.0 ], 
                                 'XSlider': ( 180.0, vcs.on ),
                                 'ZSlider': ( 0.0,   vcs.on ),
                                 'YSlider': ( 0.0,   vcs.off ), 
                                 'Camera': { 'Position': (-161, -171, 279), 'ViewUp': (.29, 0.67, 0.68), 'FocalPoint': (146.7, 8.5, -28.6)  }
                                 }  )       
    
if __name__ == '__main__':
    
    testManager.reviewTests()
    
