import vcs, cdms2, sys, os, vcs.testing.regression as regression

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
    i.datawc_x1 = 30
    i.datawc_x2 = -30
    i.datawc_y1 = -50
    i.datawc_y2 = 90
    if (zoom == 'datawc1'):
        i.datawc_x1, i.datawc_x2 = i.datawc_x2, i.datawc_x1
    if (zoom == 'datawc2'):
        i.datawc_y1, i.datawc_y2 = i.datawc_y2, i.datawc_y1
    x.plot(s, i, bg=1)

file = "test_vcs_polar_zoom_" + zoom + ".png"
regression.run(x, file)
