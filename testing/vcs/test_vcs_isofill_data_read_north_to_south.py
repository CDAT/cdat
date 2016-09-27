import os, sys, cdms2, vcs, vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
clt = f("clt",latitude=(80.0, 38.0),squeeze=1,longitude=(-180.0, 180.0),time=slice(0,1))
x = regression.init()
gm = vcs.createisofill()
gm.projection="polar"
x.plot( clt,gm,bg=1)
fnm = os.path.split(__file__)[-1][:-2]+"png"
regression.run(x, fnm)