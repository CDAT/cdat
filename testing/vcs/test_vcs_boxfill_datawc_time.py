import cdms2, os, sys, vcs, cdtime, vcs.testing.regression as regression

# Test that we can restrict the plot using datawc along a time axis
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt")
clt = clt(latitude=(-90.0, 90.0), longitude=(0.), squeeze=1,
          time=('1979-1-1 0:0:0.0', '1988-12-1 0:0:0.0'))

# Initialize canvas:
canvas = regression.init()

# Create and plot quick boxfill with default settings:
boxfill=canvas.createboxfill()

# Change the type
boxfill.boxfill_type = 'custom'
boxfill.datawc_y1 = 12

canvas.plot(clt, boxfill, bg=1)

# Load the image testing module:
# Create the test image and compare:
regression.run(canvas, "test_vcs_boxfill_datawc_time.png")
