'''
Created on Jun 18, 2014

@author: tpmaxwel
'''
import vcs, numpy
import cdms2, cdutil, genutil
import sys
import os, time
import subprocess, signal 

demo_index = '1'
background_render = 1
bgX=2048
bgY=1024

if len(sys.argv) > 1:
    demo_index = sys.argv[1]
   
x = vcs.init()
f = cdms2.open( "geos5.bkg.prs.20110501_0000z.nc4" )
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


#     shp = list( va.shape )
#     shp.append( 1 )
#     va.data.reshape( shp )
#     va.shape = shp
#    
    dv3d.VerticalScaling = 4.0 
#    dv3d.ScaleColormap = [-46.0, 46.0, 1] 
#    dv3d.ScaleTransferFunction =  [10.0, 77.0, 1]
    
else:
    print>>sys.stderr, "Unknown demo index: ", demo_index
    
x.plot( v, dv3d, bg=background_render )

if background_render: 
#     renderers = x.backend.renWin.GetRenderers()
#     renderers.InitTraversal()
#     ren = renderers.GetNextItem()
#     while ren is not None:
#         print " --- Background Color: %s " % str( ren.GetBackground() )
#         ren = renderers.GetNextItem()
    x.png( 'demo_plot-w', ignore_alpha=1 )
else:                   
    x.interact()




