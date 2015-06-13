# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,vcs,sys,os,support
bg=support.bg
fnm=os.path.join(vcs.sample_data,'clt.nc')
f=cdms.open(fnm)
s=f('clt',slice(0,1),slice(3,4),squeeze=1)
x=vcs.init()
x.plot(s,'ASD','xyvsy',bg=bg)
support.check_plot(x)
