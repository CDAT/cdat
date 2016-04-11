#!/usr/bin/env python
import cdms2
import os
import sys
import vcs

# We test if gm.datawc zooms in correctly into the plot. This works only for
# data using a linear projection. It does not work for geographic projections.
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

flip = False
if (len(sys.argv) == 3):
    flip = True

fileName = os.path.basename(__file__)
fileName = os.path.splitext(fileName)[0]
if (flip):
    fileName = fileName + '_flip'
fileName = fileName + '.png'
f=cdms2.open(os.path.join(vcs.sample_data, "sampleCurveGrid4.nc"))
s=f("sample")
x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
m=x.createmeshfill()
# m.mesh = True
m.datawc_x1 = -20
m.datawc_x2 = 20
if (flip):
    m.datawc_x1, m.datawc_x2 = m.datawc_x2, m.datawc_x1
m.datawc_y1 = -20
m.datawc_y2 = 20
x.plot(s,m, bg=1)
x.png(fileName)
ret = checkimage.check_result_image(fileName, sys.argv[1], checkimage.defaultThreshold)
sys.exit(ret)

