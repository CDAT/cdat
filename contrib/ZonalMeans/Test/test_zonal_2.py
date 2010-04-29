# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2,ZonalMeans,sys,os


## 
f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('u')
print s.shape
res = ZonalMeans.compute(s)
import vcs
import vcs.test.support
bg= vcs.test.support.bg
x=vcs.init()
x.plot(res[1],bg=bg)
vcs.test.support.check_plot(x)
lats=[-90,-80,-70,-60,-50,-40,-30,-20,-10,-5,0,5,10,20,30,40,50,60,70,80,90]
res = ZonalMeans.compute(s,delta_band=lats)
## print res

