import sys,cdutil
import vcs
import os
import cdms2
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(sys.prefix,"sample_data","ta_ncep_87-6-88-4.nc"))
ta=f("ta",time=slice(0,1),squeeze=1)
ta=cdutil.averager(ta,axis="yx")
x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.plot(ta,bg=1)
fnm = "test_oned_level_axis.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
