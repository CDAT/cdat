import os,sys,vcs,cdms2

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt")
s2=f("clt",longitude=(-180,180))
s3=f("clt",longitude=(0,360))

print s.shape,s2.shape,s3.shape

x=vcs.init()
y=vcs.init()
z=vcs.init()

b=x.getisofill("a_isofill")
x.plot(s,"default")
y.plot(s2,"default")
z.plot(s3,"default")
