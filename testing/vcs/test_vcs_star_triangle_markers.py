
import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression

x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

m = x.createmarker()

m.type = ["star", "triangle_right", "triangle_left", "triangle_up", "triangle_down"]
m.x = [[.1], [.3], [.5], [.7], [.9]]
m.y = [[.1], [.3], [.5], [.7], [.9]]
m.color = [200, 150, 160, 175, 125]
m.size = [50, 50, 50, 50, 50]
x.plot(m, bg=1)
regression.run(x, "test_star_triangle_markers.png")