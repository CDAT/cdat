import os, sys, cdms2, vcs, vcs.testing.regression as regression

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt",slice(0,1),squeeze=1)
height, width = clt.shape
clt.mask = [[True if i % 2 else False for i in range(width)] for _ in range(height)]

# Initialize canvas:
canvas = regression.init()

# Create and plot quick boxfill with default settings:
# Only have to test boxfill because all 2D methods use the same code
# for missing values
boxfill = canvas.createboxfill()

# Change the missing color to a colorname
boxfill.missing = "lightgrey"

canvas.plot(clt, boxfill, bg=1)
regression.run(canvas, "test_vcs_missing_colorname.png")
