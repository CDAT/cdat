def simpleanimation():
    import vcs, cdms2, sys
    x = vcs.init()
    f = cdms2.open(vcs.prefix+"/sample_data/clt.nc")
    v = f["clt"]
    dv3d = vcs.get3d_scalar()
    x.plot( v, dv3d )
    x.interact()

def simplevector():
    import vcs, cdms2, sys
    x = vcs.init()
    f = cdms2.open(vcs.prefix+"/sample_data/clt.nc")
    v = f["v"]
    u = f["u"]
    dv3d = vcs.get3d_vector()
    dv3d.BasemapOpacity = 0.15
    x.plot( u, v, dv3d )
    x.interact()

def simplevolume():
    import vcs, cdms2, sys
    x = vcs.init()
    f = cdms2.open(vcs.prefix+"/sample_data/geos5-sample.nc")
    u = f["uwnd"]
    dv3d = vcs.get3d_scalar()
    dv3d.VerticalScaling = 3.0
    dv3d.ScaleOpacity = [0.0, 0.8]
    dv3d.ScaleColormap = [-46.0, 45, 1]
    dv3d.ScaleTransferFunction =  [8.6, 76.7, 1]
    dv3d.BasemapOpacity = [0.5]
    dv3d.XSlider = vcs.off
    dv3d.ZSlider = vcs.off
    dv3d.YSlider = vcs.off
    dv3d.ToggleVolumePlot = vcs.on
    dv3d.ToggleSurfacePlot = vcs.off
    dv3d.Camera={'Position': (-161, -171, 279), 'ViewUp': (.29, 0.67, 0.68), 'FocalPoint': (146.7, 8.5, -28.6)}
    x.plot( u, dv3d )
    x.interact()

def run_scalar_ctest( filename, varname, parms, template = "default" ):
    import vcs, cdms2
    x = vcs.init()
    f = cdms2.open(vcs.prefix+"/sample_data/"+filename )
    v = f[varname]
    dv3d = vcs.get3d_scalar( template )
    for item in parms.items():
        dv3d.setParameter( item[0], item[1] )
    x.plot( v, dv3d )
    x.interact()

def ctest_as_script():
    import vcs
    parameters = {
					"ScaleColormap": [89.13197640956652, 100.0, 1],
					"ScaleOpacity": [1.0, 1.0],
					"BasemapOpacity": [0.5],
					"Animation": [0.0],
					"ZSlider": ( [0.2833581758795678], vcs.on ),
					"YSlider": [-90.0],
					"ToggleVolumePlot": ( [[1]], vcs.on ),
					"XSlider": [-180.0],
					"axes": [['xyt']],
					"IsosurfaceValue": [50.0],
					"VerticalScaling": [1.0],
					"ScaleTransferFunction": ( [88.42048588004492, 100.0, 1], vcs.on ),
					"Camera": {'cell': (0, 0), 'Position': (-510.89793108644596, -99.49403616328722, 499.57693223045857), 'ViewUp': (0.6679428060896573, 0.18703087988580122, 0.7203276044705059), 'FocalPoint': (0.0, 0.0, 0.0)},
					"XSlider": [-180.0],
					"YSlider": [-90.0],
					"ZSlider": ( [0.2833581758795678], vcs.on ),
					"ToggleVolumePlot": ( [[1]], vcs.on ),
					}
    run_scalar_ctest( "clt.nc", "clt", parameters,  'Hovmoller3D' )

if __name__ == "__main__":
    ctest_as_script()
