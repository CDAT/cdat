import os, sys, cdms2, vcs, vcs.testing.regression as regression

flip = False
if (len(sys.argv) == 3):
    flip = True

fileName = os.path.basename(__file__)
fileName = os.path.splitext(fileName)[0]
if (flip):
    fileName = fileName + '_flip'
fileName = fileName + '.png'
f = cdms2.open(os.path.join(vcs.sample_data, "sampleCurveGrid4.nc"))
s = f("sample")
x = regression.init()
m = x.createmeshfill()
# m.mesh = True
m.datawc_x1 = -20
m.datawc_x2 = 20
if (flip):
    m.datawc_x1, m.datawc_x2 = m.datawc_x2, m.datawc_x1
m.datawc_y1 = -20
m.datawc_y2 = 20
x.plot(s,m, bg=1)
regression.run(x, fileName)