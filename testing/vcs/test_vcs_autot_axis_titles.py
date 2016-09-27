import vcs, cdms2, os, sys, vcs.testing.regression as regression

testConfig = {'a_boxfill': ('clt.nc', 'clt'),
              'a_mollweide_boxfill': ('clt.nc', 'clt'),
              'a_meshfill': ('sampleCurveGrid4.nc', 'sample',),
              'a_robinson_meshfill': ('sampleCurveGrid4.nc', 'sample'),
              'a_lambert_isofill': ('clt.nc', 'clt'),
              'a_robinson_isoline': ('clt.nc', 'clt')}

# Tests if ratio=autot works correctly for background and foreground plots
bg = int(sys.argv[2])
plot = sys.argv[3]
x_over_y = sys.argv[4]
if (x_over_y == '0.5'):
    xSize = 250
    ySize = 500
else:
    xSize = 800
    ySize = 400
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)

f = cdms2.open(vcs.sample_data + "/" + testConfig[plot][0])
s = f(testConfig[plot][1])
x = regression.init(bg=bg, geometry=(xSize, ySize))

# graphics method
if (plot.find('boxfill') != -1):
    gm = x.getboxfill(plot)
elif (plot.find('meshfill') != -1):
    gm = x.getmeshfill(plot)
elif (plot.find('isofill') != -1):
    gm = x.getisofill(plot)
elif (plot.find('isoline') != -1):
    gm = x.getisoline(plot)
else:
    print "Invalid plot"
    sys.exit(13)

x.setantialiasing(0)
x.drawlogooff()
x.plot(s, gm, ratio="autot")
name = "test_vcs_autot_axis_titles_" + plot[2:] + "_" + x_over_y + "_" + str(bg) + ".png"
regression.run(x, name, sys.argv[1])
