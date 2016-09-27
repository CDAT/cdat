import vcs, sys, os, vcs.testing.regression as regression

x = regression.init(bg=0)
x.setantialiasing(0)
x.drawlogooff()
x.open(814,628)
x.plot([1,2,3,4,5,6,7])
fnm = __file__[:-3]+".png"
regression.run(x, fnm)
