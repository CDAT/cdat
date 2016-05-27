
import vcs, numpy, cdms2, MV2, os, sys, testing.regression as regression

x = regression.init()
data = MV2.array([4,5,6,7,1,3,7,9,])+230.
p = cdms2.createAxis([2,5,100,200,500,800,850,1000])
data.setAxis(0,p)
data.id="jim"
gm=x.create1d()
gm.linewidth=0
gm.datawc_x1=1000
gm.datawc_x2=0
gm.markersize=30
x.plot(data,gm,bg=1)
regression.run(x, "test_1d_marker_not_shown_if_xaxis_flipped.png", sys.argv[1])