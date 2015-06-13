import vcs, cdms2, sys

x = vcs.init()
f = cdms2.open( vcs.sample_data+"/geos5-sample.nc" )
dv3d = vcs.get3d_vector()
dv3d.VerticalScaling = 4.0
dv3d.BasemapOpacity = 0.5
dv3d.ScaleColormap = [50.0, 75.0, 1]
dv3d.ZSlider = [26.0], vcs.on
dv3d.GlyphDensity = 3.0
dv3d.GlyphSize = 0.6
dv3d = vcs.get3d_vector()
v0 =  f["uwnd"]
v1 =  f["vwnd"]
x.plot( v0, v1, dv3d )
x.interact()