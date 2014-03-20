# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy,cdms2 as cdms,sys,support,os
bg=support.bg

f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('clt')
g=s.getGrid()

d=s.filled()*100.

x=vcs_legacy.init()
x.plot(d,grid=g,bg=bg) # grid is just so we have the continnets
support.check_plot(x)
