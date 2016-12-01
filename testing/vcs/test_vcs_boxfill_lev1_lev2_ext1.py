import os, sys, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
f = cdms2.open(vcs.sample_data+"/clt.nc")
s = f("clt",slice(0,1),squeeze=1)
b = x.createboxfill()
b.level_1 = 20
b.level_2 = 80
b.ext_1 = "y"
x.plot(s, b, bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(x, fnm)
