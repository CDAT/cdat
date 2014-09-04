import vcs
import cdms2
import sys
import os

src=sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))
s=f("clt",slice(0,1),squeeze=1)
x=vcs.init()
i=x.createisofill()
p=x.getprojection("polar")
i.projection=p
x.open()
x.plot(s,i)
fnm= "test_polar_set_opt_param_polar.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,0.05)
sys.exit(ret)

