import vcs, numpy, cdms2, MV2, os, sys

x = vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200, 1091, units="pixels")

m = x.createmarker()

m.type = "star"
m.x = [.1]
m.y = [.1]
m.color = 200
m.size = 50
display = x.plot(m, bg=1)

success = 0
try:
    display._repr_png_()
except Exception as exc:
    print "Conversion to base64 binary string failed.", exc
    success = 1

sys.exit(success)
