
import vcs
import cdms2
import sys
import os

src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(vcs.sample_data,'clt.nc'))
s=f("clt",slice(0,1),squeeze=1)
x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
i=x.createisofill()
p=x.getprojection("polar")
i.projection=p
x.plot(s,i,bg=1)
fnm= "test_polar_set_opt_param_polar.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)

