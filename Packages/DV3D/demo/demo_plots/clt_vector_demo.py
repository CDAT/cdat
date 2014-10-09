import vcs, cdms2, sys
x = vcs.init()
f = cdms2.open(sys.prefix+"/sample_data/clt.nc")  
v = f["v"]
u = f["u"]  
dv3d = vcs.get3d_vector()
dv3d.BasemapOpacity = 0.15
x.plot( u, v, dv3d )
x.interact()

