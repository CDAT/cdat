import os, sys, cdms2, vcs, vcs.testing.regression as regression

testConfig = {'a_boxfill': ('clt.nc', 'clt', (200, 200)),
              'a_mollweide_boxfill': ('clt.nc', 'clt', (222, 322)),
              'a_isofill': ('clt.nc', 'clt', (200, 200)),
              'a_isoline': ('clt.nc', 'clt', (200, 200)),
              'vector_default': ('clt.nc', ('u', 'v'), (200, 200)),
              'a_meshfill': ('sampleCurveGrid4.nc', 'sample', (222, 322)),
              'a_robinson_meshfill': ('sampleCurveGrid4.nc', 'sample', (222, 322))}

# Tests if the info produced when clicking on a map is correct.
src = sys.argv[1]
plot = sys.argv[2]
x = regression.init(bg=False, geometry=(800, 600))

vector = False
# graphics method
if (plot.find('boxfill') != -1):
    gm = x.getboxfill(plot)
elif (plot.find('meshfill') != -1):
    gm = x.getmeshfill(plot)
elif (plot.find('isofill') != -1):
    gm = x.getisofill(plot)
elif (plot.find('isoline') != -1):
    gm = x.getisoline(plot)
elif (plot.find('vector') != -1):
    gm = x.getvector(plot[plot.index('_') + 1:])
    vector = True
else:
    print "Invalid plot"
    sys.exit(13)

# data
f = cdms2.open(vcs.sample_data + "/" + testConfig[plot][0])
if (vector):
    u = f(testConfig[plot][1][0])
    v = f(testConfig[plot][1][1])
    x.plot(u, v, gm)
else:
    s = f(testConfig[plot][1])
    x.plot(s, gm)

# Simulate a click -- VTK Specific
location = testConfig[plot][2]
i = x.backend.renWin.GetInteractor()
i.SetEventInformation(location[0], location[1])
i.LeftButtonPressEvent()

fileName = os.path.basename(src)
fileName = os.path.splitext(fileName)[0]
fileName += '.png'

regression.run(x, fileName)
