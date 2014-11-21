import vcs,numpy,cdms2,MV2

a=numpy.arange(400)
a.shape=(2,2,10,10)
b=MV2.cos(a)/2.
a=MV2.sin(a)
t=cdms2.createAxis(range(2))
t.designateTime()
t.units="months since 2014"
t.id="time"
l=cdms2.createAxis(numpy.arange(1,3)*1000.)
l.designateLevel()
l.units="hPa"
l.id="plev"
a.setAxis(0,t)
a.setAxis(1,l)
b.setAxisList(a.getAxisList())
x=vcs.init()
d = x.plot(a)
raw_input("Press enter")
x.backend.update_input(d.backend,b(slice(1,2),slice(1,2)))
raw_input("ok done")
