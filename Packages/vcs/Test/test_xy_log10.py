# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2,vcs,sys,cdutil,support,os
bg=support.bg
cdms2.setAutoBounds('on')
f=cdms2.open(os.path.join(vcs.sample_data,'clt.nc'))
s=f('clt',time=slice(10,None),longitude=(10,270))
s=cdutil.averager(s,axis='ty')
x=vcs.init()

xy=x.createxyvsy('new')
xy.yaxisconvert='log10'

x.plot(s,xy,bg=bg)
support.check_plot(x)
x.clear()
xy.yaxisconvert='linear'
x.plot(s,xy,bg=bg)
support.check_plot(x)
x.clear()
xy.yaxisconvert='log10'
x.plot(s,xy,bg=bg)
support.check_plot(x)

