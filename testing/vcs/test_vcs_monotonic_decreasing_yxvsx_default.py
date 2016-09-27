import os, sys, numpy, cdms2, MV2, vcs, vcs.testing.regression as regression

x = regression.init()
t = cdms2.createAxis(numpy.arange(120))
t.designateTime()
t.id = "time"
t.units = "months since 2014"
data = MV2.arange(120,0,-1)
data.id = "data"
data.setAxis(0,t)
x = regression.init()
x.plot(data,bg=1)
fnm = 'test_vcs_monotonic_decreasing_yxvsx_default.png'
regression.run(x, fnm)