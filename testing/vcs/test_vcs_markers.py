import os, sys, numpy, cdms2, MV2, vcs, vcs.testing.regression as regression

x = regression.init()
x.setcolormap("classic")
m = x.createmarker()
m.x = [[0.,],[5,],[10.,],[15.]]
m.y = [[0.,],[5,],[10.,],[15.]]
m.worldcoordinate=[-5,20,-5,20]

#m.worldcoordinate=[-10,10,0,10]
m.type=['plus','diamond','square_fill',"hurricane"]
m.color=[242,243,244,242]
m.size=[20,20,20,5]
x.plot(m,bg=1)
regression.run(x, "test_markers.png")
