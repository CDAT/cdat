import vcs
import cdms2
import os
import sys

f=cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))
u=f("u")
v=f("v")

V=vcs.createvector()
V.list()

x=vcs.init()
x.plot(u,v,V)


x.interact()

