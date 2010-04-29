import EzTemplate,os
import cdms2,sys,vcs
import vcs.test.support
bg= vcs.test.support.bg

f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('clt',slice(0,1))
f.close()
x=vcs.init()
M=EzTemplate.Multi(rows=2,columns=2)
for i in range(4):
    x.plot(s,M.get(),bg=bg)

y=vcs.init()
M2=EzTemplate.Multi(rows=1,columns=2)
for i in range(2):
    y.plot(s,M2.get(),bg=bg)

vcs.test.support.check_plot(y)
