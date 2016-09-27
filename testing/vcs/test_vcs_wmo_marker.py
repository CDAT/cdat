
import vcs,numpy,cdms2,MV2,os,sys


import vcs.testing.regression as regression
x = regression.init()


m = x.createmarker()
M=1
m.worldcoordinate=[0,M,0,M]
m.type = "w07"
m.color=[242,]
m.size=[1.,2.,5.]
m.x = [[.25,],[.5,],[.75]]
m.y = [.5,]
x.plot(m,bg=1)
fnm = 'wmo_marker.png'
x.png(fnm)

regression.run(x, "wmo_marker.png")

