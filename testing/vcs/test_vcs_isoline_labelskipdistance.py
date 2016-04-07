import cdms2
import os
import sys
import vcs

baselineImage = sys.argv[1]

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

dataset = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
data = dataset("clt")

canvas = vcs.init()
canvas.setantialiasing(0)
canvas.setbgoutputdimensions(1200, 1091, units="pixels")
canvas.drawlogooff()

isoline = canvas.createisoline()
isoline.label = "y"
isoline.labelskipdistance = 15.0
texts = []
colors = []
for i in range(10):
    text = canvas.createtext()
    text.color = 20 * i
    text.height = 12
    colors.append(255 - text.color)
    if i % 2 == 0:
        texts.append(text.name)
    else:
        texts.append(text)
isoline.text = texts
isoline.linecolors = colors

# Next plot the isolines with labels
canvas.plot(data, isoline, bg=1)

testImage = os.path.abspath("test_isoline_labelskipdistance.png")
canvas.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage,
                                    checkimage.defaultThreshold)

sys.exit(ret)
