
import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression

x = regression.init()
d = numpy.sin(numpy.arange(100))
d = numpy.reshape(d,(10,10))
one = x.create1d()
x.plot(d,one,bg=1)
regression.run(x, "test_vcs_1D_with_manyDs.png", sys.argv[1])
