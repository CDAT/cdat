import vcs
import cdms2
import os
import sys

src_yname = sys.argv[1]
src_xname = sys.argv[2]
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)

import checkimage

f = cdms2.open(vcs.sample_data + "/clt.nc")
s = f("clt")
x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.plot(s, bg=1, ratio="autot")
yname = "test_autot_axis_titles_yname.png"
x.png(yname)

# Work around background resize VTK bug
x.close()
x = vcs.init(bg=True, geometry=(500, 1000))
x.setantialiasing(0)
x.drawlogooff()
x.plot(s, ratio="autot")
xname = "test_autot_axis_titles_xname.png"
x.png(xname)

print "yname:", yname
print "src:", src_yname

ret = checkimage.check_result_image(yname, src_yname, checkimage.defaultThreshold)
if ret > 0:
    print "yname image did not match."
    sys.exit(ret)

print "xname:", xname
print "src:", src_xname

ret = checkimage.check_result_image(xname, src_xname, checkimage.defaultThreshold)
sys.exit(ret)
