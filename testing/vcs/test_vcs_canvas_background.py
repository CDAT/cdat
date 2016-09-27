import os, sys, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
x.backgroundcolor = (255, 255, 255)
x.open()
regression.run(x, "test_backgroundcolor_white.png")