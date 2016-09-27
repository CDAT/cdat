import os, sys, cdms2, vcs, vcs.testing.regression as regression
f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt")
x = regression.init()
iso = x.createisofill()
p=x.createprojection()
p.type="lambert"
iso.projection = p
x.plot(s(latitude=(20, 60),longitude=(-140,-20)), iso, bg=True)
regression.run(x, "test_vcs_lambert.png")