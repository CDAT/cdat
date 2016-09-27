import os, sys, cdms2, vcs, vcs.testing.regression as regression

dataset = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
data = dataset("clt")
canvas = regression.init()
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
isoline.linetypes = ('dot', 'dash', 'solid', 'dash-dot', 'long-dash', 'dot', 'dash')
# Next plot the isolines with labels
canvas.plot(data, isoline, bg=1)
regression.run(canvas, "test_vcs_isoline_width_stipple.png")
