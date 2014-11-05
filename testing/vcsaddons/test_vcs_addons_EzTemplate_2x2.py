import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

import vcs
import vcsaddons
import cdms2

f=cdms2.open(os.path.join(vcs.prefix,'sample_data','clt.nc'))
s=f("clt",time=slice(0,1),squeeze=1)

bg = True

M=vcsaddons.EzTemplate.Multi(rows=2,columns=2)
x=vcs.init()
if bg:
  x.setbgoutputdimensions(1200,1091,units="pixels")
for i in range(4):
    x.plot(s,M.get(),bg=bg)

fnm = "test_vcs_addons_EzTemplate_2x2.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
