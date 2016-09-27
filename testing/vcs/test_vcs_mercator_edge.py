import os, sys, cdms2, vcs, vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt")
x = regression.init()
iso = x.createisofill()
iso.projection = "mercator"
x.plot(s(latitude=(-90, 90)), iso, bg=1)
regression.run(x, "test_vcs_mercator_edge.png")