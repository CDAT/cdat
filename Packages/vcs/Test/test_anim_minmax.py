import sys,cdms2,vcs
x=vcs.init()
f=cdms2.open(vcs.prefix+"/sample_data/clt.nc")
s=f("clt")#,time=slice(0,3))
#f=cdms2.open(sys.prefix+"/sample_data/sampleCurveGrid4.nc")
#f=cdms2.open(sys.prefix+"/sample_data/sampleGenGrid3.nc")
#s=f("sample")
gm = x.createisofill()
gm = x.createboxfill()
#gm=x.createisoline()
levs = vcs.mkevenlevels(20,80)
gm.levels= levs
gm.fillareacolors = vcs.getcolors(levs)
#gm = x.createmeshfill()
d = x.plot(s,gm)
#d = x.plot(s.filled())
print "BE:",d.backend
raw_input("Press enter")
x.animate.create(thread_it=False)#,min=20,max=80)
x.animate.run()
raw_input("Press enter to end")
x.animate.stop()

