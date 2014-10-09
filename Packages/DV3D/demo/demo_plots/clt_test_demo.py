def simpleanimation():
    import vcs, cdms2, sys
    x = vcs.init()
    f = cdms2.open(sys.prefix+"/sample_data/clt.nc")   
    v = f["clt"] 
    dv3d = vcs.get3d_scalar()
    x.plot( v, dv3d )
    x.interact()
    
def simplevector():
    import vcs, cdms2, sys
    x = vcs.init()
    f = cdms2.open(sys.prefix+"/sample_data/clt.nc")  
    v = f["v"]
    u = f["u"]  
    dv3d = vcs.get3d_vector()
    dv3d.BasemapOpacity = 0.15
    x.plot( u, v, dv3d )
    x.interact()
        
def simplevolume():
    import vcs, cdms2, sys
    x = vcs.init()
    f = cdms2.open(sys.prefix+"/sample_data/geos5-sample.nc")  
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

if __name__ == "__main__":
    simplevolume()