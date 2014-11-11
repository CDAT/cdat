import vcs, cdms2, sys
x = vcs.init()
f = cdms2.open(sys.prefix+"/sample_data/clt.nc")   
v = f["clt"] 
dv3d = vcs.get3d_scalar('Hovmoller3D')
dv3d.ToggleSurfacePlot = vcs.on
dv3d.ToggleVolumePlot = vcs.on
dv3d.IsosurfaceValue = [11.4]
dv3d.ScaleTransferFunction = [80, 100, 1]
x.plot( v, dv3d )
x.interact()

