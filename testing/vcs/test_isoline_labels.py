import vcs,cdms2,sys,os

# ('/path/to/filename', '.extension')
baseline = os.path.splitext(sys.argv[1])

pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

dataset = cdms2.open(os.path.join(vcs.prefix,"sample_data","clt.nc"))
data = dataset("clt")

canvas = vcs.init()
canvas.setbgoutputdimensions(1200, 1091, units="pixels")
canvas.drawlogooff()

isoline = canvas.createisoline()
isoline.label="y"
texts=[]
colors = []
for i in range(10):
    text = canvas.createtext()
    text.color = 50 + 12 * i
    text.height = 12
    colors.append(100 + 12 * i)
    if i%2 == 0:
      texts.append(text.name)
    else:
      texts.append(text)
isoline.text = texts

# First test using isoline.text[...].color
canvas.plot(data, isoline, bg=1)

baselineImage = "%s%s"%baseline
testImage = os.path.abspath("test_isoline_labels.png")
canvas.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage,
                                    checkimage.defaultThreshold)

# Now set isoline.linecolors and test again.
canvas.clear()
isoline.linecolors = colors
canvas.plot(data, isoline, bg=1)

baselineImage = "%s%d%s"%(baseline[0], 2, baseline[1])
testImage = os.path.abspath("test_isoline_labels2.png")
canvas.png(testImage)

ret += checkimage.check_result_image(testImage, baselineImage,
                                     checkimage.defaultThreshold)

# Now set isoline.textcolors and test again.
canvas.clear()
isoline.textcolors = colors
canvas.plot(data, isoline, bg=1)

baselineImage = "%s%d%s"%(baseline[0], 3, baseline[1])
testImage = os.path.abspath("test_isoline_labels3.png")
canvas.png(testImage)

ret += checkimage.check_result_image(testImage, baselineImage,
                                     checkimage.defaultThreshold)

sys.exit(ret)
