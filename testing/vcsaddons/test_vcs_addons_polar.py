import sys,os
src = sys.argv[1]
import vcs.testing.regression as regression
import vcs
import vcsaddons, numpy

x = regression.init()

polar = vcsaddons.polar.Gpo()
polar.markers = ["dot", "circle"]
polar.markersizes = [3, 5]

polar.magnitude_tick_angle = numpy.pi / 6

theta = list(numpy.arange(0, 4 * numpy.pi + .01, numpy.pi / 24))
magnitude = list(numpy.sin(theta))

polar.plot(magnitude, theta, bg=True, x=x)

fnm = "test_vcs_addons_polar.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src)
sys.exit(ret)
