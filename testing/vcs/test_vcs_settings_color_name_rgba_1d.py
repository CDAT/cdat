import vcs
import numpy
import os
import sys
import cdms2
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1090,units="pixels")

f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data=f("clt")[:,5,8]
gm = x.create1d()
gm.linecolor="salmon"
gm.markercolor = [0,0,100]
x.plot(data,gm,bg=True)
fnm = 'test_vcs_settings_color_name_rgba_1d.png'
src = sys.argv[1]
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
