'''
Created on Jun 18, 2014

@author: tpmaxwel
'''
import vcs, numpy
import cdms2, cdutil, genutil
import sys
import os, time
import subprocess, signal

demo_index = '0'
background_render = 0
bgX=2048
bgY=1024
is_vector = False

if len(sys.argv) > 1:
    demo_index = sys.argv[1]

x = vcs.init()
f = cdms2.open( "/Users/tpmaxwel/Data/AConaty/comp-ECMWF/geos5.xml" ) # "geos5-sample.nc" )
if background_render:
    x.setbgoutputdimensions(width=bgX, height=bgY, units='pixels')

dv3d = vcs.get3d_scalar()
dv3d.ToggleVolumePlot = vcs.on
dv3d.ToggleSurfacePlot = vcs.off
dv3d.ScaleOpacity = [0.0, 0.8]
dv3d.BasemapOpacity = [0.5]
dv3d.XSlider = ( vcs.off )
dv3d.ZSlider = ( vcs.off )
dv3d.YSlider = ( vcs.off )
dv3d.Camera={'Position': (-161, -171, 279), 'ViewUp': (.29, 0.67, 0.68), 'FocalPoint': (146.7, 8.5, -28.6)}

dv3d_v = vcs.get3d_vector()
#dv3d_v.Camera={'Position': (-161, -171, 279), 'ViewUp': (.29, 0.67, 0.68), 'FocalPoint': (146.7, 8.5, -28.6)}

if demo_index == '0':
    varname = "sphu"
    v = f[varname]
    dv3d = vcs.get3d_scalar()
    dv3d.VerticalScaling = 7.0
    dv3d.ScaleColormap = [.00129, 0.0055, 1]
    dv3d.ScaleTransferFunction =  [0.0015, 0.0088, 1]

elif demo_index == '1':
    varname = "uwnd"
    v = f[varname]
    dv3d.VerticalScaling = 4.0
    dv3d.ScaleColormap = [-46.0, 46.0, 1]
    dv3d.ScaleTransferFunction =  [10.0, 77.0, 1]

elif demo_index == '2':
    varname = "vwnd"
    v = f[varname]
    dv3d.VerticalScaling = 4.0
    dv3d.ScaleColormap = [-46.0, 46.0, 1]
    dv3d.ScaleTransferFunction =  [10.0, 77.0, 1]

elif demo_index == '3':
    varname = "tmpu"
    v0 = f[varname]
    va = cdutil.averager( v0, axis='x' )
    v01,va1=genutil.grower(v0,va)
    v = v01 - va1

    dv3d.ScaleColormap = [-17.0, 14.7, 1]
    dv3d.ScaleTransferFunction =  [ 3.64, 24, 1]

elif demo_index == '4':
    v0 =  f["uwnd"]
    v1 =  f["vwnd"]
    is_vector = True
    dv3d_v.VerticalScaling = 4.0
    dv3d_v.BasemapOpacity = 0.0

elif demo_index == '5':
    varname = "tmpu"
    v = f[varname]
    dv3d.ToggleVolumePlot = vcs.off

else:
    print>>sys.stderr, "Unknown demo index: ", demo_index

if is_vector:
    x.plot( v0, v1, dv3d_v, bg=background_render )
else:
    x.plot( v, dv3d, bg=background_render )

if background_render:
#     renderers = x.backend.renWin.GetRenderers()
#     renderers.InitTraversal()
#     ren = renderers.GetNextItem()
#     while ren is not None:
#         print " --- Background Color: %s " % str( ren.GetBackground() )
#         ren = renderers.GetNextItem()
    x.png( 'demo_plot', ignore_alpha=1 )
else:
    x.interact()




