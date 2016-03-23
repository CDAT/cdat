import vcs
import sys
import os
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

data = [1,10,100,1000,10000,100000]

x=vcs.init(geometry={"width":800,"height":600})
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
gm = x.create1d()
gm.yaxisconvert = "log10"
gm.yticlabels1 = { 1:"1",10:"10",100:"100",1000:"1000",10000:"10000",10000:"100000"}
ticks = {}
for i in range(5):
    ticks[50*(i+1)] = "%s" % (50*(i+1))
gm.ymtics2 = ticks
x.plot(data,gm,bg=True)

fnm = "test_vcs_1d_log10_axes.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
