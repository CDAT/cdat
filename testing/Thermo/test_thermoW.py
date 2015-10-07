import cdms2
import os
import sys
import thermo
import vcs

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage  # noqa

test_data_dir = sys.argv[1]
baselineImage = os.path.join(test_data_dir, "baselines", "Thermo",
                             "test_thermoW.png")

bg = True

x = vcs.init()
x.setantialiasing(0)
x.portrait()
th = thermo.Gth(x=x, name='test')

# List setable stuff
# th.list()

# Type of thermodynamic diagram
# th.type='emagram'
# th.type='tephigram'
# th.type='stuve'
th.type = 'skewT'

# Skewness of the plot
# th.skewness=-35.

# Graphic finess
th.detail = 75

# World Coordinates
# Temperatures at the bottom of the grap (in C)
th.datawc_x1 = -50.
th.datawc_x2 = 50.
# Pressure at bottom and top of page (in hPa)
th.datawc_y1 = 1050.
th.datawc_y2 = 100.


# Drawing of paper
# th.isotherms.level=vcs.mkscale(-200,200)
th.drawisothermsfilled = 1
th.drawisotherms = 1
th.drawisobars = 1
th.drawdryadiabats = 1
th.drawpseudoadiabats = 1
th.drawmixingratio = 1

# Create a template for the windbarbs
template = x.createtemplate('new2')
template.data.x1 = .86
template.data.x2 = .96
template.data.y1 = .1
template.data.y2 = .9
template.box1.x1 = template.data.x1
template.box1.x2 = template.data.x2
template.box1.y1 = template.data.y1
template.box1.y2 = template.data.y2
template.xlabel1.y = template.data.y1 * .9
template.ylabel1.x = template.data.x1 * .9

# Read winds from a file
f = cdms2.open(os.path.join(vcs.sample_data, 'thermo.nc'))
u = f('ua', time=slice(0, 1), longitude=0, latitude=0, squeeze=1)
v = f('va', time=slice(0, 1), longitude=0, latitude=0, squeeze=1)

# Windbarbs scale (triangle, long bar, small bar)
th.windbarbsscales = [5, 2, 1]
# Plot the windbarbs, passing the P values (in Pa)

th.plot_windbarb(u, v, P=u.getLevel()[:] * 100., template=template, bg=bg)

fnm = "test_thermoW.png"
th.x.png(fnm)
ret = checkimage.check_result_image(fnm, baselineImage,
                                    checkimage.defaultThreshold)
sys.exit(ret)
