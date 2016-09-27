import os, sys, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt")
x.meshfill(s, bg=1)
regression.run(x, "test_vcs_meshfill_regular_grid.png")
