import vcs,numpy,cdms2,MV2

a=numpy.arange(6000)
a.shape=(12,5,10,10)
b=MV2.cos(a)/2.
a=MV2.sin(a)
t=cdms2.createAxis(range(12))
t.designateTime()
t.units="months since 2014"
t.id="time"
l=cdms2.createAxis(numpy.arange(1,6)*1000.)
l.designateLevel()
l.units="hPa"
l.id="plev"
a.setAxis(0,t)
a.setAxis(1,l)
b.setAxisList(a.getAxisList())
x=vcs.init()
d = x.plot(a)
raw_input("Press enter")
print d
x.backend.update_input(d.backend,b(slice(1,2),slice(1,2)))
raw_input("ok done")
