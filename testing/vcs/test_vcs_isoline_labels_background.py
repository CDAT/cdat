import cdms2
import os
import sys
import vcs
import random

# ('/path/to/filename', '.extension')
baseline = os.path.splitext(sys.argv[1])

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

(latmin, latmax, lonmin, lonmax) = (-90, 90, -180, 180)
dataset = cdms2.open(os.path.join(vcs.sample_data, "tas_cru_1979.nc"))
data = dataset("tas", time=slice(0, 1), latitude=(latmin, latmax),
               longitude=(lonmin, lonmax, 'co'), squeeze=1)
dataset.close()

canvas = vcs.init()
canvas.setantialiasing(0)
canvas.setbgoutputdimensions(1200, 1091, units="pixels")
canvas.drawlogooff()
canvas.backgroundcolor = [100, 105, 105]

isoline = canvas.createisoline()
isoline.label = "y"
texts = []
colors = []
bcolors = []
bopacities = []
for i in range(10):
    text = canvas.createtext()
    random.seed(i*200)
    text.color = random.randint(1, 255)
    text.height = 12
    random.jumpahead(i * 100)
    colors.append(random.randint(1, 255))
    random.jumpahead(i * 20)
    bcolors.append(random.randint(1, 255))
    bopacities.append(random.randint(0, 100))
    if i % 2 == 0:
        texts.append(text.name)
    else:
        texts.append(text)
isoline.text = texts
isoline.labelbackgroundcolors = bcolors
isoline.labelbackgroundopacities = bopacities
isoline.labelskipdistance = 15.0

# First test using isoline.text[...].color
canvas.plot(data, isoline, bg=1)

baselineImage = "%s%s" % baseline
testImage = os.path.abspath("test_isoline_labels_background.png")
canvas.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage,
                                    checkimage.defaultThreshold)
sys.exit(ret)
