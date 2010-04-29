import vcsaddons,os
import vcs.test.support
bg=vcs.test.support.bg

h = vcsaddons.createyxvsxfill()

import sys,cdms2,MV2,cdutil
cdms2.setAutoBounds("on")
f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f("clt")

d1=MV2.max(s,axis=0)
d2=MV2.min(s,axis=0)


s1 = cdutil.averager(d1,axis='x')
s2 = cdutil.averager(d2,axis='x')

yf = vcsaddons.createyxvsxfill()


yf.plot(s1,s2,bg=bg)
vcs.test.support.check_plot(yf.x)

