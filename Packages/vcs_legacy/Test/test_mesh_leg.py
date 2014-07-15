# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy,cdms2 as cdms,sys,support,os
bg=support.bg

f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','sampleCurveGrid4.nc'))
s=f('sample')

x=vcs_legacy.init()
t=x.createtemplate('jj')
m=x.createmeshfill('hh')
m.mesh='y'

m=x.createisofill('jj')

t.scale(.8)
t.legend.y2=.8

t.legend.x1=.8
t.legend.x2=.82

x.plot(s,t,m,bg=bg)
support.check_plot(x)
x.clear()

t.legend.x2=.78
t.legend.x1=.8
x.plot(s,m,t,bg=bg)
support.check_plot(x)
x.clear()

t.legend.y2=t.legend.y1
t.legend.y1=.8
x.plot(s,m,t,bg=bg)
support.check_plot(x)
x.clear()

t.legend.x1=.2
t.legend.x2=.8
t.legend.y1=.15
t.legend.y2=.2
x.plot(s,m,t,bg=bg)
support.check_plot(x)

x.clear()
t.legend.y1=.15
t.legend.y2=.1
x.plot(s,m,t,bg=bg)
support.check_plot(x)
