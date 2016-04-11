import vcs
import cdms2
import os
import sys

testConfig = {'a_boxfill': ('clt.nc', 'clt'),
              'a_mollweide_boxfill': ('clt.nc', 'clt'),
              'a_meshfill': ('sampleCurveGrid4.nc', 'sample',),
              'a_robinson_meshfill': ('sampleCurveGrid4.nc', 'sample'),
              'a_lambert_isofill': ('clt.nc', 'clt'),
              'a_robinson_isoline': ('clt.nc', 'clt')}

# Tests if ratio=autot works correctly for background and foreground plots
src = sys.argv[1]
bg = 1
if (sys.argv[2] == 'foreground'):
    bg = 0
plot = sys.argv[3]
x_over_y = sys.argv[4]
if (x_over_y == '0.5'):
    xSize = 400
    ySize = 800
else:
    xSize = 800
    ySize = 400
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)

import checkimage

f = cdms2.open(vcs.sample_data + "/" + testConfig[plot][0])
s = f(testConfig[plot][1])

x = vcs.init(bg=bg, geometry=(xSize, ySize))

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
name = "test_autot_axis_titles_" + plot[2:] + "_" + x_over_y + "_" + str(bg) + ".png"
x.png(name)

print "name:", name
print "src:", src

ret = checkimage.check_result_image(name, src, checkimage.defaultThreshold)
sys.exit(ret)
