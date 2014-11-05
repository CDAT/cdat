import cdms2,sys,vcs,os

x=vcs.init()
x.open()

b=vcs.createboxfill()

p=vcs.createprojection()
#p.type="polar (non gctp)"
p.type=-3

b.projection = p

f=cdms2.open(os.path.join(vcs.prefix,"sample_data","clt.nc"))
s=f("clt",slice(0,1),longitude=(0,360),squeeze=1)
#s=f("clt",slice(0,1),squeeze=1)

#x.plot(s,b,ratio="1t")
x.plot(s,b)
raw_input()

x.png("vcs_test_polar")


