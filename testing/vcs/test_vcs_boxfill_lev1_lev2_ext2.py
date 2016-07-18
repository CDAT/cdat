import os, sys, cdms2, vcs, testing.regression as regression

x = regression.init()
f = cdms2.open(vcs.sample_data+"/clt.nc")
s = f("clt",slice(0,1),squeeze=1)
b = x.createboxfill()
b.level_1 = 20
b.level_2 = 80
b.ext_2 = "y"
x.plot(s, b, bg=1)
regression.run(x, "test_boxfill_lev1_lev2_ext2.png")