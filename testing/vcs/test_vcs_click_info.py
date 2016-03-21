import cdms2
import sys
import vcs
import os

testConfig = {'a_boxfill': ('clt.nc', 'clt', (200, 200)),
              'a_mollweide_boxfill': ('clt.nc', 'clt', (222, 322)),
              'a_meshfill': ('sampleCurveGrid4.nc', 'sample', (222, 322)),
              'a_robinson_meshfill': ('sampleCurveGrid4.nc', 'sample', (222, 322))}

# Tests if the info produced when clicking on a map is correct.
src = sys.argv[1]
plot = sys.argv[2]
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage
x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
# Needs to set the size of window so it is consistent accross
# test platforms
x.open(814, 606)

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
    exit(13)

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

x.png(fileName, width=814, height= 606)

ret = checkimage.check_result_image(fileName, src, checkimage.defaultThreshold)
sys.exit(ret)
