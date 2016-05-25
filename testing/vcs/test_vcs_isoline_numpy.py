import os, sys, cdms2, vcs, testing.regression as regression

x = regression.init()
x.setantialiasing(0)
x.setbgoutputdimensions(1200,1091,units="pixels")
x.drawlogooff()
fnm = os.path.join(vcs.sample_data,'clt.nc')
f = cdms2.open(fnm)
s = f("clt")
gm = x.createisofill()
x.plot(s.filled(),gm,bg=1)
fnm = "test_vcs_isoline_numpy.png"
regression.run(x, fnm)