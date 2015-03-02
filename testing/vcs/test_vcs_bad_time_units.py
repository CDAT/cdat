import cdms2,vcs
import os,sys

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",slice(0,1))
s.getTime().units="XXX-))rvv"
x=vcs.init()
x.plot(s,bg=1)
