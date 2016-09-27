import os, sys, cdms2, vcs, vcs.testing.regression as regression

dataset = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data = dataset("clt")
canvas = regression.init()
isofill = canvas.createisofill()
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

# Plot the isolines with labels
canvas.plot(data, isoline, bg=1)
regression.run(canvas, "test_vcs_isofill_isoline_labels.png")
