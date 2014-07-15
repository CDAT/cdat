import vcs_legacy,vcs_legacy.test.support

x=vcs_legacy.init()
bg=vcs_legacy.test.support.bg
p0 = x.createfillarea()
p1 = x.createmarker()
p2 = x.createline()
p3 = x.createtext()

p1.type='cross'
p1.color=242
p1.size=10
p0.color=244
p3.string=['C0','C1','C2','C3']

for p in [p0,p1,p2,p3]:
    p.viewport=[.02,.98,.02,.98]
    p.worldcoordinate=[-180,180,-90,90]
    p.x=[-175,-175,175,175]
    p.y=[-88,88,88,-88]
    x.plot(p,ratio=1,bg=bg)
    vcs_legacy.test.support.check_plot(x)
    x.clear()

