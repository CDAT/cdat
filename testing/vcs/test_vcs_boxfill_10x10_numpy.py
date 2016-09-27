import vcs, numpy, os, sys, vcs.testing.regression as regression

s = numpy.sin(numpy.arange(100))
s = numpy.reshape(s,(10,10))
x = regression.init()
x.plot(s, bg=1)
regression.run(x, "test_vcs_boxfill_10x10_numpy.png")