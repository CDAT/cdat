import os, sys, cdms2, vcs, vcs.testing.regression as regression
import matplotlib
sp = matplotlib.__version__.split(".")
if int(sp[0])*10+int(sp[1])<15:
    # This only works with matplotlib 1.5 and greater
    sys.exit()

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt")
clt = clt(latitude=(-90.0, 90.0), longitude=(-180., 175.), squeeze=1,
          time=('1979-1-1 0:0:0.0', '1988-12-1 0:0:0.0'))

# Initialize canvas:
canvas = regression.init()
canvas.setcolormap(vcs.matplotlib2vcs("viridis"))
canvas.plot(clt, bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(canvas, fnm)
