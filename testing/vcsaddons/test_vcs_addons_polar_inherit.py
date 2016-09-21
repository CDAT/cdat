import sys,os
src = sys.argv[1]
import vcs.testing.regression as regression
import vcs
import vcsaddons, numpy

x = regression.init()

gm = vcsaddons.polar.Gpo()
gm.markers = ["dot", "circle"]
gm.markersizes = [3, 5]
gm.markercolors = ["red", "blue"]
gm.clockwise = True
gm.theta_offset = numpy.pi / 4
gm.magnitude_ticks = [.2 * i for i in range(6)]
gm.magnitude_tick_angle = numpy.pi / 10
gm.theta_tick_count = 10
gm.group_names = ["First", "Second"]

polar = vcsaddons.polar.Gpo(source=gm)

assert polar.markersizes == gm.markersizes
assert polar.markercolors == gm.markercolors
assert polar.markers == gm.markers
assert polar.clockwise == gm.clockwise
assert polar.theta_offset == gm.theta_offset
assert polar.magnitude_ticks == gm.magnitude_ticks
assert polar.magnitude_tick_angle == gm.magnitude_tick_angle
assert polar.theta_tick_count == gm.theta_tick_count
assert polar.group_names == gm.group_names

polar.magnitude_tick_angle = numpy.pi / 6

theta = list(numpy.arange(0, 4 * numpy.pi + .01, numpy.pi / 24))
magnitude = list(numpy.sin(theta))

theta = [theta[:len(theta) / 2], theta[len(theta) / 2:]]
magnitude = [magnitude[:len(magnitude)/ 2], magnitude[len(magnitude) / 2:]]

polar.plot(magnitude, theta, bg=True, x=x)

fnm = "test_vcs_addons_polar_inherit.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src)
sys.exit(ret)
