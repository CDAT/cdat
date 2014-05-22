import vcs

x=vcs.init()

m = x.createmarker()
m.x=[[0.,],[5,],[10.,]]
m.y=[[0.,],[5,],[10.,]]
m.worldcoordinate=[0,10,0,10]
#m.worldcoordinate=[-10,10,0,10]
m.type=['plus','diamond','square_fill']
m.type=['hurricane',]
m.color=[242,243,244]
m.size=[10.,]
m.list()
x.plot(m)
x.png('marker')

raw_input()

