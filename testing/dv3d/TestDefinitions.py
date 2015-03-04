'''
Created on Aug 28, 2014

@author: tpmaxwel
'''
from TestManager import *
import vcs, sys, os

testManager = TestManager()

vcsTest( "Hovmoller_volume_test", file="clt.nc", vars=['clt'], type="3d_scalar", template="Hovmoller3D",
				parameters = {
					"ScaleColormap": {0: 89.13197640956652, 1: 100.0, 2: 1, 'Slice': [17.27143292788585, 100.0, 1], 'Surface': [17.27143292788585, 100.0, 1], 'Volume': [89.13197640956652, 100.0, 1], 'state': 0},
					"ChooseColormap": {'state': 0},
					"ScaleOpacity": {0: 1.0, 1: 1.0, 'Slice': [1.0, 1.0], 'Surface': [1.0, 1.0], 'Volume': [1.0, 1.0], 'state': 0},
					"BasemapOpacity": {0: 0.5, 'state': 0},
					"Animation": {0: 0.0, 'state': 0},
					"ZSlider": {'relative': 0.2833581758795678, 0: 0.2833581758795678, 'state': 1, 'absolute': -79.40784756275343},
					"YSlider": {0: -90.0},
					"ScaleTransferFunction": {0: 88.42048588004492, 1: 100.0, 2: 1, 'state': 1},
					"Colorbar": {'state': 0},
					"ToggleVolumePlot": {0: [1], 'init': [1], 'state': 1},
					"XSlider": {0: -180.0},
					"ToggleClipping": {'state': 0},
					"Camera": {'Position': (-510.89793108644596, -99.49403616328722, 499.57693223045857), 'ViewUp': (0.6679428060896573, 0.18703087988580122, 0.7203276044705059), 'FocalPoint': (0.0, 0.0, 0.0)},
					"IsosurfaceValue": {0: 50.0, 'state': 0},
					"VerticalScaling": {0: 1.0, 'state': 0},
					} )

vcsTest( "dv3d_vector_test", file="geos5-sample.nc", vars=['uwnd', 'vwnd'], type="3d_vector", template="default",
				parameters = {
					"GlyphDensity": {0: 6.0, 'state': 0},
					"ScaleColormap": {0: -45.960629, 1: 76.746857, 2: 1, 'Slice': [-45.960629, 76.746857, 1], 'Surface': [-45.960629, 76.746857, 1], 'Volume': [-45.960629, 76.746857, 1], 'state': 0},
					"ChooseColormap": {'state': 0},
					"GlyphSize": {0: 0.3, 'state': 0},
					"ScaleOpacity": {0: 1.0, 1: 1.0, 'Slice': [1.0, 1.0], 'Surface': [1.0, 1.0], 'Volume': [1.0, 1.0], 'state': 0},
					"BasemapOpacity": {0: 0.5, 'state': 0},
					"Camera": {'Position': (180.0, 0.0, 540.0), 'ViewUp': (0.0, 1.0, 0.0), 'FocalPoint': (180.0, 0.0, 0.0)},
					"ZSlider": {'relative': 0.6755583954359683, 0: 0.6755583954359683, 'state': 1, 'absolute': 242.52546396151263},
					"ScaleTransferFunction": {0: -45.960629, 1: 76.746857, 2: 1, 'state': 0},
					"Colorbar": {'state': 0},
					"ToggleClipping": {'state': 0},
					"Animation": {0: 0.0, 'state': 0},
					"IsosurfaceValue": {0: 15.39311408996582, 'state': 0},
					"VerticalScaling": {0: 1.0, 'state': 0},
					} )


vcsTest( 'dv3d_slider_test', roi=( -105.0, -15.0, 5.0, 50.0 ), file="geos5-sample.nc", vars = [ 'uwnd' ],
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
     
vcsTest( 'dv3d_volume_test', roi=( -105.0, -15.0, 7.0, 50.0 ), file="geos5-sample.nc", vars = [ 'uwnd' ],
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


vcsTest( 'dv3d_surface_test', roi=( -105.0, -15.0, 5.0, 50.0 ), file="geos5-sample.nc", vars = [ 'uwnd' ],
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

# vcsTest( 'dv3d_constituents_test', file="geos5-sample.nc", vars = [ 'uwnd' ], type="3d_scalar", template="default",
#                      parameters={'VerticalScaling': {0: 7.0, 'init': 7.0, 'state': 0},
#                                  'ToggleClipping': ( 40, 360, -28, 90 ),
#                                  'ToggleVolumePlot': {'state': 1, 'ConfigEnabled': 0},
#                                  'ToggleSurfacePlot': {'state': 1, 'ConfigEnabled': 0},
#                                  'ScaleOpacity': {0: 0.44139420931096296, 1: 1.0,  'init': [0.0, 0.8], 'Volume': [0.0, 0.3], 'state': 0},
#                                  'IsosurfaceValue': {0: 31.0, 'state': 0},
#                                  'ScaleColormap': {0: -46.0, 1: 48.0, 2: 1, 'init': [-46.0, 48.0, 1], 'Surface': [ 30.0, 35.0, 1], 'Volume': [16.0, 30.0, 1], 'state': 1},
#                                  'BasemapOpacity':  {0: 0.5, 'init': [0.5], 'state': 0},
#                                  'ScaleTransferFunction': {0: 12.0, 1: 77.0, 'init': [12.0, 77.0, 1], 2: 1, 'state': 0},
#                                  'XSlider':  {0: 180.0, 'state': 1},
#                                  'ZSlider':  {0: 0.01, 'relative': 0.01, 'state': 0, 'absolute': 3.59},
#                                  'YSlider':  {0: 0.0, 'state': 0},
#                                  'Camera': { 'Position': (-161, -171, 279), 'ViewUp': (.29, 0.67, 0.68), 'FocalPoint': (146.7, 8.5, -28.6)  }
#                                  }  )

vcsTest( "dv3d_constituents_test", file="geos5-sample.nc", vars=['uwnd'], type="3d_scalar", template="default",
				parameters = {
					"ScaleColormap": {0: -45.96, 1: -9.67, 2: 1, 'Slice': [-31.36, 28.85, 1], 'Surface': [-45.96, -9.67, 1], 'Volume': [-38.6, 55.3, 1], 'state': 0},
					"ChooseColormap": {'state': 0},
					"ToggleSurfacePlot": {'state': 1, 'ConfigEnabled': 1},
					"ScaleOpacity": {0: 1.0, 1: 1.0, 'Slice': [1.0, 1.0], 'Surface': [1.0, 1.0], 'Volume': [1.0, 1.0], 'state': 0},
					"BasemapOpacity": {0: 0.5, 'state': 0},
					"Animation": {0: 0.0, 'state': 0},
					"ZSlider": {'relative': 0.4007016592985228, 0: 0.4007016592985228, 'state': 1, 'absolute': 406.6946427055376},
					"YSlider": {0: 61.9468932651198, 'state': 1},
					"ScaleTransferFunction": {0: 12.678084180448273, 1: 76.746857, 2: 1, 'state': 0},
					"Colorbar": {'state': 0},
					"ToggleVolumePlot": {'state': 1, 'ConfigEnabled': 0},
					"XSlider": {0: 240.01411753347577, 'state': 1},
					"ToggleClipping": {'state': 0},
					"Camera": {'Position': (-256.1, -182.2, 269.8), 'ViewUp': (0.40, 0.41, 0.82), 'FocalPoint': (148.7, 15.4, -27.8)},
					"IsosurfaceValue": {0: 29.0491548788426, 'state': 0},
					"VerticalScaling": {0: 2.565944033040266, 'state': 0},
					} )

vcsTest( 'dv3d_hovmoller_test', file="clt.nc", vars = [ 'clt' ], template = 'Hovmoller3D',
                     parameters={'BasemapOpacity': 0.5,
                                 'XSlider': ( 77.8, vcs.on ),
                                 'ZSlider': ( 0.1,   vcs.on ),
                                 'YSlider': ( 49.0,   vcs.on ),
                                 'Camera': {'Position': (-300, -409, 400), 'ViewUp': (0.156, 0.64, 0.75), 'FocalPoint': (-23.2, 14.2, -20)}
                                 }  )



if __name__ == '__main__':

    from TestManager import *
    testManager.reviewTest('dv3d_constituents_test')
