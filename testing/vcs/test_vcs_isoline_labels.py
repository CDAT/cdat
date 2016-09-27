import os, sys, cdms2, vcs, vcs.testing.regression as regression

dataset = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data = dataset("clt")
canvas = regression.init()
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

baseline = os.path.splitext(sys.argv[1])
baselineImage = "%s%s"%baseline
ret = regression.run_wo_terminate(canvas, "test_vcs_isoline_labels.png", baselineImage)

# Now set isoline.linecolors and test again.
canvas.clear()
isoline.linecolors = colors
canvas.plot(data, isoline, bg=1)
baselineImage = "%s%d%s"%(baseline[0], 2, baseline[1])
testImage = os.path.abspath("test_vcs_isoline_labels2.png")
ret += regression.run_wo_terminate(canvas, testImage, baselineImage)

# Now set isoline.textcolors and test again.
canvas.clear()
isoline.textcolors = colors
canvas.plot(data, isoline, bg=1)

baselineImage = "%s%d%s"%(baseline[0], 3, baseline[1])
testImage = os.path.abspath("test_vcs_isoline_labels3.png")
ret += regression.run_wo_terminate(canvas, testImage, baselineImage)

sys.exit(ret)
