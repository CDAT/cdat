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

boxfill = canvas.createboxfill()

boxfill.color_1 = 242
boxfill.color_2 = 250

canvas.plot(data, boxfill, bg=1)

testImage = os.path.abspath("test_fewer_colors_than_levels.png")
canvas.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage, checkimage.defaultThreshold)

sys.exit(ret)
