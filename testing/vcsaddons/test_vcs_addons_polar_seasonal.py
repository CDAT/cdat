import sys,os
src = sys.argv[1]
import vcs.testing.regression as regression
import vcs
import vcsaddons, numpy, MV2
import cdms2, cdutil, cdtime

x = regression.init()

f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
# Trim first few months and last month so we have even number of seasons
cloudiness = f('clt', time=(11, 119))
cdutil.setAxisTimeBoundsMonthly(cloudiness.getTime())
cloudiness_time_axis = cloudiness.getTime()
averaged_seasons = MV2.zeros((36, 46, 72))
# Average the seasons in cloudiness
for i in range(36):
    averaged_seasons[i] = cdutil.averager(cloudiness(time=(cloudiness_time_axis[i * 3], cloudiness_time_axis[(i+1) * 3])), axis="t")

averaged_seasons.setAxis(1, cloudiness.getLatitude())
averaged_seasons.setAxis(2, cloudiness.getLongitude())

regions = {
    "north_polar": (66, 90),
    "north_temperate": (22, 66),
    "tropics": (-22, 22),
    "south_temperate": (-66, -22),
    "south_polar": (-90, -66)
}

def get_region_avg(var, r, axis="xy"):
    avg = cdutil.averager(var(latitude=regions[r]), axis=axis)
    avg.id = r
    return avg

magnitudes = [get_region_avg(averaged_seasons, region) for region in regions]
thetas = [range(4) * 27] * 5

polar = vcsaddons.getpolar("seasonal")
polar.datawc_y1 = 0
polar.datawc_y2 = 100
polar.markers = ["dot"]
polar.markersizes = [3]
polar.markercolors = vcs.getcolors([-90, -66, -22, 22, 66, 90], split=False)

polar.magnitude_tick_angle = numpy.pi / 4

polar.plot(magnitudes, thetas, bg=True, x=x)

fnm = "test_vcs_addons_polar_seasonal.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src)
sys.exit(ret)
