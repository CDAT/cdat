import cdms2
import os
import sys
import vcs

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt")
clt = clt[0]
height, width = clt.shape
clt.mask = [[True if i % 2 else False for i in range(width)] for _ in range(height)]

# Initialize canvas:
canvas = vcs.init()
canvas.setantialiasing(0)
canvas.setbgoutputdimensions(1200,1091,units="pixels")
canvas.drawlogooff()

# Create and plot quick boxfill with default settings:
# Only have to test boxfill because all 2D methods use the same code
# for missing values
boxfill = canvas.createboxfill()

# Change the missing color to a colorname
boxfill.missing = "Medium Aquamarine"

canvas.plot(clt, boxfill, bg=1)

# Load the image testing module:
testingDir = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(testingDir)
import checkimage

# Create the test image and compare:
baseline = sys.argv[1]
testFile = "test_vcs_missing_colorname.png"
canvas.png(testFile)
ret = checkimage.check_result_image(testFile, baseline,
                                    checkimage.defaultThreshold)
sys.exit(ret)
