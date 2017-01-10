import os, sys, cdms2, vcs, vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s = f("clt",slice(0,1),squeeze=1)
x = regression.init()
gm = x.createboxfill()
gm.boxfill_type = "custom"
gm.levels = [1.e20,1.e20]
gm.ext_1 = "y"
gm.ext_2 = "y"
x.plot(s, gm, bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(x, fnm, sys.argv[1])
