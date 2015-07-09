import cdms2
import os
import sys
import vcs

# Load the clt vector data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
u = dataFile("u", slice(0, 4))
v = dataFile("v", slice(0, 4))

# Initialize canvas:
canvas = vcs.init()
canvas.setbgoutputdimensions(600, 400, units="pixels")
canvas.drawlogooff()
canvas.setantialiasing(0)

# Create vector gm
gm = canvas.createvector()

# Plot and animate
canvas.plot(u, v, gm) #bg=1)
canvas.animate.create()
prefix = os.path.split(__file__)[1][:-3]
mp4 = prefix + ".mp4"
canvas.animate.save(mp4)
pngs = canvas.animate.close(preserve_pngs = True)

# Load the image testing module:
testingDir = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(testingDir)
import checkimage

# Compare frames to baselines:
baselinePath = os.path.join(sys.argv[1], prefix)
result = 0
for png in pngs:
    baseline = os.path.join(baselinePath, os.path.split(png)[1])
    result += checkimage.check_result_image(png, baseline,
                                            checkimage.defaultThreshold)

# Clean up if tests pass
if len(pngs) > 0 and result == 0:
    os.removedirs(os.path.split(pngs[0])[0])
    os.remove(mp4)

canvas.close()
sys.exit(result)
