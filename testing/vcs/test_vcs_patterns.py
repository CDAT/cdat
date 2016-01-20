import cdms2
import os
import sys
import vcs

src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

f = cdms2.open(vcs.sample_data+"/clt.nc")
s = f("clt", time=slice(0, 1), squeeze=1)
x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200, 1090, units="pixels")
iso = vcs.createisofill("isoleg")
iso.levels = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
iso.fillareastyle = "pattern"
iso.fillareacolors = vcs.getcolors([0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100])
iso.fillareaindices = [1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
x.plot(s, iso, bg=1)
fnm = "test_vcs_patterns.png"
x.png(fnm)

print "fnm:", fnm
print "src:", src
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold+5.)
sys.exit(ret)
