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
data=f("clt",slice(0,1,))
gm = x.createisoline()
gm.levels = range(0,110,10)
gm.linecolors = ["green","red","blue","bisque","yellow","grey",
        [100,0,0,50], [0,100,0],"salmon",[0,0,100,75]]
x.plot(data,gm,bg=True)
fnm = 'test_vcs_settings_color_name_rgba_isoline.png'
src = sys.argv[1]
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
