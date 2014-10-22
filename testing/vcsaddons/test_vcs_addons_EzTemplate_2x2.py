import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

import vcs
import cdms2
import MV2

bg = True

x=vcs.init()
if bg:
  x.setbgoutputdimensions(1200,1091,units="pixels")
fnm = "test_vcs_addons_EzTemplate_2x2.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
