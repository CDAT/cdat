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

polar = vcsaddons.polar.Gpo()
polar.markers = ["dot", "circle"]
polar.markersizes = [3, 5]

polar.magnitude_tick_angle = numpy.pi / 6

theta = list(numpy.arange(0, 4 * numpy.pi + .01, numpy.pi / 24))
magnitude = list(numpy.sin(theta))

polar.plot(magnitude, theta, bg=True, x=x)

fnm = "vcs_addons_test_polar.png"
x.png(fnm)
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)
sys.exit(ret)
