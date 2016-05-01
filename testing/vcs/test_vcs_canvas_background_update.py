import os, sys, cdms2, vcs, testing.regression as regression

x = regression.init()
x.backgroundcolor = (255, 255, 255)
x.open()
x.backgroundcolor = (255, 255, 0)
x.update()
regression.run(x, "test_backgroundcolor_yellow.png")