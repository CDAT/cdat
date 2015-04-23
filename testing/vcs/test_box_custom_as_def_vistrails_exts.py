import vcs
import cdms2
import os
import sys
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(vcs.prefix,"sample_data","clt.nc"))
s=f("clt",slice(0,1),squeeze=1)

x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

gm=x.createboxfill()
gm.boxfill_type="custom"
gm.levels=[1.e20,1.e20]
gm.ext_1="y"
gm.ext_2="y"

x.plot(s,gm,bg=1)
fnm = "test_box_custom_as_def_vistrails_exts.png"
src =sys.argv[1]
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
