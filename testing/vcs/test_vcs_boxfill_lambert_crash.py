#!/usr/bin/env python
import cdms2, os, sys, vcs, vcs.testing.regression as regression

f = cdms2.open(sys.argv[2])
a = f("Z3")

x = regression.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200, 900, units="pixels")
p=x.getprojection("lambert")
b=x.createboxfill()
b.projection=p
x.setbgoutputdimensions(1200,1091,units="pixels")
x.plot(a(latitude=(20,60),longitude=(-160,-120)),b, bg=1)

fileName = os.path.basename(__file__)
fileName = os.path.splitext(fileName)[0]
fileName += '.png'
ret = regression.run(x, fileName)
