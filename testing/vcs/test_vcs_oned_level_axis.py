import os, sys, vcs, cdms2, cdutil, vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"ta_ncep_87-6-88-4.nc"))
ta = f("ta",time=slice(0,1),squeeze=1)
ta = cdutil.averager(ta,axis="yx")
x = regression.init()
x.plot(ta,bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(x, fnm)
