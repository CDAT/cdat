import vcs,cdms2,numpy,MV2

t=cdms2.createAxis(numpy.arange(120))
t.designateTime()
t.id="time"
t.units="months since 2014"
data = MV2.arange(120,0,-1)
data.id="data"
data.setAxis(0,t)
x=vcs.init()
x.plot(data)
raw_input("Press enter")
