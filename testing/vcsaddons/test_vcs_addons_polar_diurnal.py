import sys,os
src = sys.argv[1]
import vcs
import vcsaddons, numpy
import cdms2, cdutil, cdtime
import vcs.testing.regression as regression

x = regression.init()

f = cdms2.open(os.path.join(vcs.sample_data, "thermo.nc"))
temp = f('t')
levels = temp.getLevel()
time = temp.getTime()
# Break up temp by level
magnitudes = [temp[:,i] for i in range(temp.shape[1])]
for i, mag in enumerate(magnitudes):
    mag.id = "%0.f %s" % (levels[i], levels.units)

times = []
for t in time:
    reltime = cdtime.relativetime(t, time.units)
    comptime = reltime.tocomponent()
    times.append(comptime.hour)

thetas = [times] * len(magnitudes)

polar = vcsaddons.getpolar("diurnal")
polar.markers = ["dot"]
polar.markersizes = [3]
polar.markercolors = vcs.getcolors(list(levels))

polar.magnitude_tick_angle = numpy.pi / 8

polar.plot(magnitudes, thetas, bg=True, x=x)

fnm = "test_vcs_addons_polar_diurnal.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src)
sys.exit(ret)
