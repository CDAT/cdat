import sys,os
import vcs.testing.regression as regression
import vcs
import vcsaddons, numpy

src = sys.argv[1]

x = regression.init()

polar = vcsaddons.getpolar("annual_cycle")
polar.markers = ["dot"]
polar.markersizes = [3]

polar.magnitude_tick_angle = numpy.pi / 8

import cdms2, cdutil

f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = f("clt")
cdutil.setAxisTimeBoundsMonthly(clt.getTime())
averaged_time = cdutil.averager(clt, axis="t")
averaged_time = averaged_time.reshape((1, averaged_time.shape[0], averaged_time.shape[1]))
averaged_time_for_departures = numpy.repeat(averaged_time, len(clt), axis=0)

clt_departures = clt - averaged_time_for_departures
clt_departures.setAxisList(clt.getAxisList())
avg_departures = cdutil.averager(clt_departures, axis="xy")

theta = range(1, len(clt) + 1)
magnitude = avg_departures
polar.plot(magnitude, theta, bg=True, x=x)

fnm = "test_vcs_addons_polar_annual.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src)
sys.exit(ret)
