import vcs
import sys
import os

src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.open(814,628)
x.plot([1,2,3,4,5,6,7])
fnm = __file__[:-3]+".png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
