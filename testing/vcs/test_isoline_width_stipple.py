import cdms2
import os
import sys
import vcs

baselineImage = sys.argv[1]

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage  # noqa

dataset = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
data = dataset("clt")

canvas = vcs.init()
canvas.setantialiasing(0)
canvas.setbgoutputdimensions(1200, 1091, units="pixels")
canvas.drawlogooff()

isoline = canvas.createisoline()
isoline.label = "y"
texts = []
colors = []
levels = []
for i in range(7):
    levels.append(i*10)
    text = canvas.createtext()
    text.color = 255 - 20 * i
    text.height = 12
    colors.append(10 + 10 * i)
    if i % 2 == 0:
        texts.append(text.name)
    else:
        texts.append(text)
isoline.levels = levels
isoline.text = texts
isoline.linecolors = colors

isoline.linewidths = (1, 2, 3, 4, 1)
isoline.line = ('dot', 'dash', 'solid', 'dash-dot', 'long-dash', 'dot', 'dash')

# Next plot the isolines with labels
canvas.plot(data, isoline, bg=1)

testImage = os.path.abspath("test_isoline_width_stipple.png")
canvas.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage, 30)
sys.exit(ret)
