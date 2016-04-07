#!/usr/bin/env python
import cdms2 
import os 
import sys
import vcs

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage


f=cdms2.open(sys.argv[2])
h=f("heat")
x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200, 900, units="pixels")

x.plot(h, bg=1)
fnm = "vcs_test_meshfill_no_wrapping.png"
x.png(fnm)
ret = checkimage.check_result_image(fnm, sys.argv[1], checkimage.defaultThreshold)
sys.exit(ret)
