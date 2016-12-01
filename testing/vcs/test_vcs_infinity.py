import os, sys, numpy, MV2, cdms2, vcs.testing.regression as regression

s= numpy.sin(numpy.arange(100))
s = numpy.reshape(s,(10,10))

s[4,6] = numpy.inf
s[7,9] = numpy.NINF
s[9,2] = numpy.nan

x = regression.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.plot(s,bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(x, fnm)
