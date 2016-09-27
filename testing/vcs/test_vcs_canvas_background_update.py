import os, sys, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
x.backgroundcolor = (255, 255, 255)
x.open()
x.backgroundcolor = (255, 255, 0)
x.update()
regression.check_result_image(x, "test_backgroundcolor_yellow.png")