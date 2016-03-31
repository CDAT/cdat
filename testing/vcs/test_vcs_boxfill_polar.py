#!/usr/bin/env python
import cdms2
import os
import sys
import vcs

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

f = cdms2.open(vcs.sample_data + "/clt.nc")
a=f("clt")

x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200, 900, units="pixels")

p=x.getprojection("polar")
b=x.createboxfill()
b.projection=p
#b.datawc_y1 = 90
#b.datawc_y2 = -90

x.setbgoutputdimensions(1200,1091,units="pixels")
x.plot(a(latitude=(90,-90)), b, bg=1)

fileName = os.path.basename(__file__)
fileName = os.path.splitext(fileName)[0]
fileName += '.png'
x.png(fileName)
ret = checkimage.check_result_image(fileName, sys.argv[1], checkimage.defaultThreshold)
sys.exit(ret)
