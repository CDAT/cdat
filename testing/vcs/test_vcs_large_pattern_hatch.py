import vcs
import sys
import os

baseline = sys.argv[1]

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

canvas = vcs.init()
canvas.setantialiasing(0)
canvas.setbgoutputdimensions(1200,1090, units="pixels")
canvas.drawlogooff()

fillarea = vcs.createfillarea()
fillarea.x = [[0, .33, .33, 0], [.33, .67, .67, .33], [.67, 1, 1, .67]]
fillarea.y = [[0, 0, 1, 1]] * 3
fillarea.style = ["solid", "pattern", "hatch"]
fillarea.index = [1, 5, 5]
fillarea.color = [50, 50, 50]

canvas.plot(fillarea, bg=True)

testImage = os.path.abspath("test_vcs_large_pattern_hatch.png")
canvas.png(testImage)

ret = checkimage.check_result_image(testImage, baseline,
                                    checkimage.defaultThreshold)

sys.exit(ret)
