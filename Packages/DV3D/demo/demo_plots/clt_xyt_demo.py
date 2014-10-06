import vcs, cdms2, sys
x = vcs.init()
f = cdms2.open(sys.prefix+"/sample_data/clt.nc")   
v = f["clt"] 
dv3d = vcs.get3d_scalar('xyt')
x.plot( v, dv3d )
x.interact()

