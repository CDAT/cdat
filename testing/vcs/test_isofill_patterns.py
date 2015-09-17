# Test the use of patterns for isofill plots

import cdms2
import os
import sys
import vcs

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

baselineImage = sys.argv[1]

# Zone that we want to plot
#
# NOTE: the (latmin, latmax, lonmin, lonmax) information HAS TO be the
# same in the variable, the 'isof' isofill method and the 2 'cont_*'
# continents plotting methods! Otherwise, the data will not match the
# continents that are plotted over it...
(latmin, latmax, lonmin, lonmax) = (-90, 90, -180, 180)

# Read one time step (the first one) from the data file
# and explicitely specify the lat/lon range we need. cdms2
# will retrieve the data in the order we want, regardless of the way
# it is stored in the data file
dataset = cdms2.open(os.path.join(vcs.sample_data, "tas_ccsr-95a_1979.01-1979.12.nc"))
v = dataset("tas", time=slice(0, 1), latitude=(latmin, latmax),
            longitude=(lonmin, lonmax, 'co'), squeeze=1)
dataset.close()

# Initialize the graphics canvas
x = vcs.init()
x.setantialiasing(0)
x.setbgoutputdimensions(1200, 1091, units="pixels")

# Create the isofill method
isof = x.createisofill('test_isofill_patterns')
isof.datawc(latmin, latmax, lonmin, lonmax)
isof.levels = [220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320]
isof.fillareastyle = 'pattern'
isof.fillareacolors = [242, 244, 237, 248, 250, 252, 44, 243, 139, 247]  # Colors
isof.fillareaindices = [1, 3, 5, 7, 9, 11, 18, 15, 17, 19]
isof.fillareaopacity = [50, 75, 20, 0, 25, 30, 100, 0, 60, 0]

# Plot the test data
#
# We have to make sure the data and the continents are plotted at the
# same place ('data' area) on the canvas, by using the same template!
tpl = x.createtemplate('tpl', 'default')
x.plot(tpl, isof, v, continents=True, bg=1)

testImage = os.path.abspath("test_isofill_patterns.png")
x.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage,
                                    checkimage.defaultThreshold)

sys.exit(ret)
