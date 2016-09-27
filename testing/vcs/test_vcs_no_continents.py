import os, sys, cdms2, vcs, vcs.testing.regression as regression

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt")
clt = clt(latitude=(-90.0, 90.0), longitude=(-180., 175.), squeeze=1,
          time=('1979-1-1 0:0:0.0', '1988-12-1 0:0:0.0'))

# Initialize canvas:
canvas = regression.init()

t1 = vcs.createtemplate()
t1.scale(.5, "y")
t1.move(-.15, "y")
t2 = vcs.createtemplate(source=t1.name)
t2.move(.5, 'y')

canvas.plot(clt, t1, continents=0, bg=True)
canvas.plot(clt, t2, continents=1, bg=True)

regression.run(canvas, "test_vcs_no_continents.png")