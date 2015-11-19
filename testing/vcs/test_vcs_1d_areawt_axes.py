import vcs
import cdms2
import os
import sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data = f("clt",slice(0,1),slice(0,None),slice(32,33),squeeze=1)


x=vcs.init(geometry={"width":800,"height":600})
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
gm = x.create1d()
gm.xaxisconvert = "area_wt"
ticks = {}
for i in range(10):
    v = 10*i
    if v != 0:
        ticks[-v] = "%sS" % v
        ticks[v] = "%sN" % v
    else:
        ticks[v] = "Eq"
gm.xticlabels1 = ticks
x.plot(data,gm,bg=True)
fnm = "test_vcs_1d_areawt_axes.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
