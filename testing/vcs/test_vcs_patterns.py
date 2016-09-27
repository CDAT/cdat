import os, sys, vcs, cdms2, vcs.testing.regression as regression

f = cdms2.open(vcs.sample_data+"/clt.nc")
s = f("clt", time=slice(0, 1), squeeze=1)
x = regression.init()
iso = vcs.createisofill("isoleg")
iso.levels = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
iso.fillareastyle = "pattern"
iso.fillareacolors = vcs.getcolors([0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100])
iso.fillareaindices = [1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
x.plot(s, iso, bg=1)
fnm = "test_vcs_patterns.png"
regression.run(x, fnm, threshold=regression.defaultThreshold+5.)