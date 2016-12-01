import cdms2, os, sys, vcs, vcs.testing.regression as regression

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt")
clt = clt(latitude=(-90.0, 90.0), longitude=(-180., 175.), squeeze=1,
          time=('1979-1-1 0:0:0.0', '1988-12-1 0:0:0.0'))

# Initialize canvas:
canvas = regression.init()

# Create and plot quick boxfill with default settings:
boxfill=canvas.createboxfill()

# Change the type
boxfill.boxfill_type = 'custom'

canvas.plot(clt, boxfill, bg=1)

# Load the image testing module:
# Create the test image and compare:
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(canvas, fnm)
