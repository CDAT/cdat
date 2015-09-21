import vcs,cdms2
import os,sys
f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt")
x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
iso = x.createisofill()
iso.projection = "mercator"
x.plot(s(latitude=(-90, 90)), iso, bg=1)

# Load the image testing module:
testingDir = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(testingDir)
import checkimage

# Create the test image and compare:
baseline = sys.argv[1]
testFile = "test_vcs_mercator_edge.png"
x.png(testFile)
ret = checkimage.check_result_image(testFile, baseline,
                                    checkimage.defaultThreshold)
sys.exit(ret)
