import os, sys, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
fnm = os.path.join(vcs.sample_data,'clt.nc')
f = cdms2.open(fnm)
s = f("clt")
gm = x.createisofill()
x.plot(s.filled(),gm)
fnm = "test_vcs_isoline_numpy.png"
regression.run(x, fnm)
