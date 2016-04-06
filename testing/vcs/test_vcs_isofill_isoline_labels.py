import vcs,cdms2,sys,os

baselineImage = sys.argv[1]

pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

dataset = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data = dataset("clt")

canvas = vcs.init()
canvas.setantialiasing(0)
canvas.setbgoutputdimensions(1200, 1091, units="pixels")
canvas.drawlogooff()

isofill = canvas.createisofill()

# First plot the isofill
canvas.plot(data, isofill, bg=1)

isoline = canvas.createisoline()
isoline.label="y"
texts=[]
colors = []
for i in range(10):
    text = canvas.createtext()
    text.color = 255 - 20 * i
    text.height = 12
    colors.append(60 + 5 * i)
    if i%2 == 0:
      texts.append(text.name)
    else:
      texts.append(text)
isoline.text = texts
isoline.linecolors = colors

# Next plot the isolines with labels
canvas.plot(data, isoline, bg=1)

testImage = os.path.abspath("test_isofill_isoline_labels.png")
canvas.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage,
                                    checkimage.defaultThreshold)

sys.exit(ret)
