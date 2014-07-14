# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,vcs_legacy,sys,os,support
bg=support.bg
fnm=os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc')
f=cdms.open(fnm)
s=f('clt',slice(0,1),slice(3,4),squeeze=1)
x=vcs_legacy.init()
x.plot(s,'ASD','xyvsy',bg=bg)
support.check_plot(x)
