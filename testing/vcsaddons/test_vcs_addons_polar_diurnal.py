import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import vcs
import vcsaddons, numpy
import cdms2, cdutil, cdtime

x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

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

fnm = "vcs_addons_test_polar_diurnal.png"
x.png(fnm)
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)
sys.exit(ret)
