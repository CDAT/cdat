import cdms2
import os
import sys
import vcs

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt")
clt = clt(latitude=(-90.0, 90.0), longitude=(-180., 175.), squeeze=1,
          time=('1979-1-1 0:0:0.0', '1988-12-1 0:0:0.0'))

# Initialize canvas:
canvas = vcs.init()
canvas.setantialiasing(0)
canvas.setbgoutputdimensions(1200,1091,units="pixels")
canvas.drawlogooff()

t1 = vcs.createtemplate()
t1.scale(.5, "y")
t1.move(-.15, "y")
t2 = vcs.createtemplate(source=t1.name)
t2.move(.5, 'y')

canvas.plot(clt, t1, continents=0, bg=True)
canvas.plot(clt, t2, continents=1, bg=True)

# Load the image testing module:
testingDir = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(testingDir)
import checkimage

# Create the test image and compare:
baseline = sys.argv[1]
testFile = "test_vcs_no_continents.png"
canvas.png(testFile)
ret = checkimage.check_result_image(testFile, baseline,
                                    checkimage.defaultThreshold)
sys.exit(ret)
