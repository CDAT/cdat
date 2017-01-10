# Test if the marker can be removed (marker=None) from a 1D plot
# Works in 1.5.1, but fails in 2.1
#
# J-Y Peterschmitt - LSCE - 03/2015

import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression

dummy_data = numpy.arange(50, dtype=numpy.float32)
x = regression.init()
gm = x.createyxvsx('test_yxvsx')

gm.marker = None
x.plot(gm, dummy_data,bg=1)
fnm = "test_vcs_remove_marker_none_1d.png"
regression.run(x, fnm)
