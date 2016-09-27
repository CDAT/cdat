import os, sys, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
f = cdms2.open(vcs.sample_data+"/clt.nc")
s = f("clt",slice(0,1),squeeze=1)
b = x.createboxfill()
b.level_1 = .5
b.level_2 = 14.5
x.plot(s, b, bg=1)
regression.run(x, "test_vcs_boxfill_lev1_lev2.png")
