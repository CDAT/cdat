import vcsaddons,vcs,os
import vcs.test.support
bg=vcs.test.support.bg

x=vcs.init()

h = vcsaddons.createyxvsxfill(x=x)

import sys,cdms2,MV2,cdutil
cdms2.setAutoBounds("on")
f=cdms2.open(os.path.join(vcs.sample_data,'clt.nc'))
s=f("clt")

d1=MV2.max(s,axis=0)
d2=MV2.min(s,axis=0)


s1 = cdutil.averager(d1,axis='x')
s2 = cdutil.averager(d2,axis='x')

yf = vcsaddons.createyxvsxfill()


x.plot(s1,s2,yf,bg=bg)
vcs.test.support.check_plot(x)

