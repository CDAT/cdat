import os, sys, cdms2, vcs, vcs.testing.regression as regression

baselineName = sys.argv[1]
projection = sys.argv[2]
zoom = sys.argv[3]

f = cdms2.open(vcs.sample_data + "/clt.nc")
a = f("clt")

x = regression.init()
p = x.getprojection(projection)
b = x.createboxfill()
b.projection = p
if (zoom == 'none'):
    x.plot(a(latitude=(90,-90)), b, bg=1)
elif (zoom == 'subset'):
    x.plot(a(latitude=(-50,90), longitude=(30, -30)), b, bg=1)
else:
    b.datawc_x1 = 30
    b.datawc_x2 = -30
    b.datawc_y1 = -50
    b.datawc_y2 = 90
    x.plot(a, b, bg=1)

fileName = os.path.basename(baselineName)
fileName = os.path.splitext(fileName)[0]
fileName += '.png'

regression.run(x, fileName)
