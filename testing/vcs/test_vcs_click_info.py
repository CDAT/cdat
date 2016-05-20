import os, sys, cdms2, vcs, testing.regression as regression

testConfig = {'a_boxfill': ('clt.nc', 'clt', (200, 200)),
              'a_mollweide_boxfill': ('clt.nc', 'clt', (222, 322)),
              'a_meshfill': ('sampleCurveGrid4.nc', 'sample', (222, 322)),
              'a_robinson_meshfill': ('sampleCurveGrid4.nc', 'sample', (222, 322))}

# Tests if the info produced when clicking on a map is correct.
src = sys.argv[1]
plot = sys.argv[2]
x = regression.init(bg=False, geometry=(800, 600))

# data
f = cdms2.open(vcs.sample_data + "/" + testConfig[plot][0])
s = f(testConfig[plot][1])

# graphics method
if (plot.find('boxfill') != -1):
    gm = x.getboxfill(plot)
elif (plot.find('meshfill') != -1):
    gm = x.getmeshfill(plot)
else:
    print "Invalid plot"
    sys.exit(13)

# Has to plot in foreground to simulate a click
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