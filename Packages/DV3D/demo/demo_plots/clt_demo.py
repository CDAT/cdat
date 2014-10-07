import cdms2, sys
import Packages.vcs as vcs

x = vcs.init()
f = cdms2.open(sys.prefix+"/sample_data/clt.nc")   
v = f["clt"] 
dv3d = vcs.get3d_scalar()
x.plot( v, dv3d )
x.interact()

