import vcs,cdms2,sys,os

baselineImage = sys.argv[1]

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
for i in range(20):
    text = canvas.createtext()
    text.color=230+i
    text.height = 12
    colors.append(100+i)
    if i%2 == 0:
      texts.append(text.name)
    else:
      texts.append(text)
isoline.text = texts
isoline.textcolors = colors

canvas.plot(data, isoline, bg=1)

testImage = "test_isoline_labels.png"
canvas.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage,
                                    checkimage.defaultThreshold)
sys.exit(ret)
