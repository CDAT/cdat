import sys
src = sys.argv[1]
import vcs.testing.regression as regression
import vcs
import vcsaddons, numpy

x = regression.init()

polar = vcsaddons.getpolar("degrees")
polar.markers = ["dot", "circle"]
polar.markersizes = [3, 5]

polar.magnitude_tick_angle = numpy.pi / 6

theta = numpy.array(range(0, 720, 2))
magnitude = 9 * numpy.sin(5 * 2 * numpy.pi * theta / 360)
polar.datawc_y1 = 0
polar.datawc_y2 = max(magnitude)
polar.plot(magnitude, theta, bg=True, x=x)

fnm = "test_vcs_addons_polar_degrees.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src)
sys.exit(ret)
