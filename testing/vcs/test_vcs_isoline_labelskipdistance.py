import os, sys, cdms2, vcs, vcs.testing.regression as regression

dataset = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
data = dataset("clt")
canvas = regression.init()

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
regression.run(canvas, "test_vcs_isoline_labelskipdistance.png")
