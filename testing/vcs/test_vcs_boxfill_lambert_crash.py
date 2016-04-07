#!/usr/bin/env python
import cdms2
import os
import sys
import vcs

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

f = cdms2.open(sys.argv[2])
a=f("Z3")

x=vcs.init()
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
x.png(fileName)
ret = checkimage.check_result_image(fileName, sys.argv[1], checkimage.defaultThreshold)
sys.exit(ret)
