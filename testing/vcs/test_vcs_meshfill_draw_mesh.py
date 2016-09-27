import os, sys, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
fnmcurv = os.path.join(vcs.sample_data,'sampleCurveGrid4.nc')
f = cdms2.open(fnmcurv)
s = f("sample")
m = x.createmeshfill()
m.mesh = True

x.plot(s,m,bg=1)
regression.run(x, "test_vcs_meshfill_draw_mesh.png")
