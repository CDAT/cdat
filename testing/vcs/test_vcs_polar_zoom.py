import vcs, cdms2, sys, os, testing.regression as regression

zoom = sys.argv[2]

f = cdms2.open(os.path.join(vcs.sample_data,'clt.nc'))
s = f("clt",slice(0,1),squeeze=1)
x = regression.init()
i = x.createisofill()
p = x.getprojection("polar")
i.projection=p
if (zoom == 'none'):
    x.plot(s,i,bg=1)
elif (zoom == 'subset'):
    x.plot(s(latitude=(-50,90), longitude=(30, -30)), i, bg=1)
else:
    s.datawc_x1 = -50
    s.datawc_x2 = 90
    s.datawc_y1 = 30
    s.datawc_y2 = -30
    x.plot(s, i, bg=1)
file = "test_vcs_polar_zoom_" + zoom + ".png"
regression.run(x, file)
