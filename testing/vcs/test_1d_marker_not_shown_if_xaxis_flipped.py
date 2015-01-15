import vcs
import numpy
import MV2
import cdms2
import sys
import os

src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

data = MV2.array([4,5,6,7,1,3,7,9,])+230.

p = cdms2.createAxis([2,5,100,200,500,800,850,1000])

data.setAxis(0,p)

data.id="jim"

gm=x.create1d()

gm.linewidth=0
gm.datawc_x1=1000
gm.datawc_x2=0

gm.markersize=30

x.plot(data,gm,bg=1)

fnm = "test_1d_marker_not_shown_if_xaxis_flipped.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
