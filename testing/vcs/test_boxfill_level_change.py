import cdms2
import os
import sys
import vcs

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.prefix, "sample_data/clt.nc"))
clt = dataFile("clt")
clt = clt(latitude=(-90.0, 90.0), longitude=(-180., 175.), squeeze=1,
          time=('1979-1-1 0:0:0.0', '1988-12-1 0:0:0.0'))

# Initialize canvas:
canvas = vcs.init()
canvas.drawlogooff()

# Create and plot quick boxfill with default settings:
boxfill = canvas.getboxfill('quick')
canvas.plot(clt, boxfill, bg=1)

# Change the levels and replot:
boxfill.boxfill_type = 'custom'
boxfill.color_1 = 16
boxfill.color_2 = 239
boxfill.level_1 = 0.0
boxfill.level_2 = 20.0

canvas.plot(clt, boxfill, bg=1)

# Load the image testing module:
testingDir = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(testingDir)
import checkimage

# Create the test image and compare:
baseline = sys.argv[1]
testFile = "test_boxfill_level_change.png"
canvas.png(testFile)
ret = checkimage.check_result_image(testFile, baseline,
                                    checkimage.defaultThreshold)
sys.exit(ret)
