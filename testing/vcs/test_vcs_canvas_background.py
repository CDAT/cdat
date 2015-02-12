import vcs, cdms2, os, sys

pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x = vcs.init()

x.drawlogooff()
x.setbgoutputdimensions(500,500, units="pixels")

x.backgroundcolor = (255, 255, 255)
x.open()
fnm = "test_backgroundcolor_white.png"
x.png(fnm)

src=sys.argv[1]
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)

sys.exit(ret)
