import cdms2
import vcs
import sys
import os
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
clt = f("clt",latitude=(80.0, 38.0),squeeze=1,longitude=(-180.0, 180.0),time=slice(0,1))
x = vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.setantialiasing(0)
gm = vcs.createisofill()
gm.projection="polar"
x.plot( clt,gm,bg=1)
fnm = os.path.split(__file__)[-1][:-2]+"png"
src= sys.argv[1]
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
