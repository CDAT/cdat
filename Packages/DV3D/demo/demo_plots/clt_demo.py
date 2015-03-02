import vcs, cdms2, sys

x = vcs.init()
f = cdms2.open(vcs.prefix+"/sample_data/clt.nc")
v = f["clt"]
dv3d = vcs.get3d_scalar()
x.plot( v, dv3d )
x.interact()

