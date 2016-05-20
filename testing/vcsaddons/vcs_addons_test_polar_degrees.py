import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import vcs
import vcsaddons, numpy

x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

polar = vcsaddons.getpolar("degrees")
polar.markers = ["dot", "circle"]
polar.markersizes = [3, 5]

polar.magnitude_tick_angle = numpy.pi / 6

theta = numpy.array(range(0, 720, 2))
magnitude = 9 * numpy.sin(5 * 2 * numpy.pi * theta / 360)
polar.datawc_y1 = 0
polar.datawc_y2 = max(magnitude)
polar.plot(magnitude, theta, bg=True, x=x)

fnm = "vcs_addons_test_polar_degrees.png"
x.png(fnm)
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)
sys.exit(ret)
