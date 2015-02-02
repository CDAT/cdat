
import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

import vcsaddons

bg = True

M=vcsaddons.EzTemplate.Multi(rows=2,columns=2)
if bg:
  M.x.setbgoutputdimensions(1200,1091,units="pixels")
fnm = "test_vcsaddons_preview_2x2.png"
M.preview(out=fnm,bg=bg)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
if not bg:
    raw_input("Press Enter")
sys.exit(ret)
