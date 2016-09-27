import os, sys, cdms2, vcs, vcs.testing.regression as regression

dataset = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data = dataset("clt")

canvas = regression.init()

boxfill = canvas.createboxfill()

boxfill.color_1 = 242
boxfill.color_2 = 250
boxfill.colormap = "classic"

canvas.plot(data, boxfill, bg=1)

regression.run(canvas, "test_fewer_colors_than_levels.png")
