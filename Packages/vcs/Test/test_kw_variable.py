# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs,cdms2,sys,support,os
bg=support.bg

f=cdms2.open(os.path.join(vcs.sample_data,'clt.nc'))
s=f('clt',time=slice(0,2))
g=s.getGrid()

d=s.filled()*100.

print 'grid:',g
x=vcs.init()
x.plot(variable=d,grid=g,bg=bg) # grid is just so we have the continnets
support.check_plot(x)
