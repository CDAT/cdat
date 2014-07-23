import cdms2,sys,vcs,os

x=vcs.init()

b=vcs.createboxfill()

p=vcs.createprojection()
p.type="polar"

b.projection = p

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",slice(0,1),squeeze=1)

x.plot(s,b)

x.png("vcs_test_polar")
x.interact()


