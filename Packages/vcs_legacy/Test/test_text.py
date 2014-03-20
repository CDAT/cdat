# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy,cdms2 as cdms,os,sys,support
bg=support.bg

tp = vcs_legacy.init()

tp.portrait()

fnm=os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc')
f=cdms.open(fnm)
s=f('clt')
tp.plot(s,bg=bg)
support.check_plot(tp)
tit = tp.createtext('tit')
tit.x = [0.5]
tit.y=[.9]
tit.halign = 'center'
tit.height = 20
tit.color=241
n=6
i=0
tit.string = 'Line %d - Decorrelation Time of Volume Mean Temperature Change' % i
tp.plot(tit,bg=bg)
support.check_plot(tp)
tp.mode=1
for i in range(1,n):
  tit.y = [1.-(i/float(n))]
  support.check_plot(tp)
