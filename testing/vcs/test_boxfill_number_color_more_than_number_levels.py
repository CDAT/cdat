import cdms2
import os
import sys
import vcs
import warnings
warnings.filterwarnings('error')

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.prefix, "sample_data/clt.nc"))
clt = dataFile("clt")
clt = clt(latitude=(-90.0, 90.0), longitude=(-180., 175.), squeeze=1,
          time=('1979-1-1 0:0:0.0', '1988-12-1 0:0:0.0'))

# Initialize canvas:
canvas = vcs.init()
canvas.setbgoutputdimensions(1200,1091,units="pixels")
canvas.drawlogooff()

# Create and plot quick boxfill with default settings:
boxfill=canvas.createboxfill()

# Change the type
boxfill.boxfill_type = 'custom'
levels = range(20,81,10)
boxfill.levels=levels
boxfill.ext_1="y"
levels = range(20,81,5)
boxfill.fillareacolors=vcs.getcolors(levels)

try:
    canvas.plot(clt, boxfill, bg=1)
    failed = False
except Warning:
    failed = True
except:
    failed = False

if not failed:
    raise RuntimeError("This test did not issue warning as expected")
