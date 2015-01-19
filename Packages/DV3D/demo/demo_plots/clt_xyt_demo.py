import vcs, cdms2, sys
x = vcs.init()
f = cdms2.open(sys.prefix+"/sample_data/clt.nc")   
v = f["clt"] 
dv3d = vcs.get3d_scalar('Hovmoller3D')
dv3d.ToggleSurfacePlot = vcs.on
dv3d.ToggleVolumePlot = vcs.on
dv3d.IsosurfaceValue = [ 10.0  ]
dv3d.ToggleClipping = [-180.0, 175.0, -22.0, 90.0,  0.0, 119.0 ]
dv3d.ScaleTransferFunction = [80, 100, 1]
dv3d.ScaleOpacity={ 'Volume': [0.0, 0.2] }
dv3d.ZSlider = ( 0.0, vcs.on )
dv3d.YSlider = ( 20.0, vcs.on )
dv3d.Camera={'Position': (436.8, -126.3, 285.2), 'ViewUp': (-0.5, 0.25, 0.83), 'FocalPoint': (9.6, 19.9, -3.2)}

x.plot( v, dv3d )
x.interact()

