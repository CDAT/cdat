# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs,cdms2 as cdms,sys,support,os
bg=support.bg

f=cdms.open(os.path.join(vcs.sample_data,'clt.nc'))
s=f('clt',time=slice(0,2))

x=vcs.init()
x.plot(s,yarray=s.getAxis(-2)[:]/2.,bg=bg)
support.check_plot(x)
