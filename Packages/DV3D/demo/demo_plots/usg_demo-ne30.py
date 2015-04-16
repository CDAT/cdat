import vcs, cdms2, os
plot_index = 2
data_dir =  os.path.expanduser( "~/Data/Unstructured_ne30/" )

x = vcs.init()
dv3d = vcs.get3d_scalar()

if plot_index == 0:
    filename = "ne30_gx1.B1850c5d.cam.h0.0005-01.nc"
    varname = "U"
    dv3d.ScaleColormap = [26.2, 50.2]
    dv3d.ScaleOpacity = [0.3, 1.0]
    dv3d.Camera = {'Position': (-862, -444, 793), 'ViewUp': (0.52, 0.35, 0.78), 'FocalPoint': (5.6, -2.0, 13.5)}
    dv3d.ScaleTransferFunction = [15.3, 50.7]

if plot_index == 1:
    filename = "ne30_gx1.B1850c5d.cam.h0.0005-01.nc"
    varname = "RELHUM"
    dv3d.ScaleColormap = [78,124]
    dv3d.ScaleOpacity = [0.53, 1.0]
    dv3d.Camera = {'Position': (647, -467, 221), 'ViewUp': (-0.23, 0.16, 0.96), 'FocalPoint': (5.7, 4.35, -9.5)}
    dv3d.ScaleTransferFunction = [90,136]

if plot_index == 2:
    filename = "ne30_gx1.B1850c5d_JAN_climo.nc"
    varname = "RELHUM"

f = cdms2.open( os.path.join( data_dir, filename ) )
v = f[ varname ]

dv3d.PointSize = [8, 6]
dv3d.VerticalScaling = [ 1.7 ]
dv3d.ToggleVolumePlot = vcs.on
dv3d.ToggleSphericalProj = vcs.on
x.plot( v, dv3d )
x.interact()
