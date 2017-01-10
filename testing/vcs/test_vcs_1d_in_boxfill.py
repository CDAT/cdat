
import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression

x = regression.init()
d = numpy.sin(numpy.arange(100))
b = x.createboxfill()
x.plot(d,b,bg=1)
regression.run(x, "test_vcs_1d_in_boxfill.png", sys.argv[1])
