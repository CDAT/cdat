import vcs, os, sys, cdms2, vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"sampleCurveGrid4.nc"))
s = f("sample")
x = regression.init()
x.plot(s,bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(x, fnm)
