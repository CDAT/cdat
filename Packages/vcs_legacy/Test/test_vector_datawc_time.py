# Adapted for numpy/ma/cdms2 by convertcdms.py
import sys,cdms2 as cdms,vcs_legacy,cdtime,support,os
bg=support.bg

t0=cdtime.comptime(1999)
t1=cdtime.comptime(2005)
f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','test.xml'))

s=f('u',latitude=slice(5,6),squeeze=1)
s2=f('v',latitude=slice(5,6),squeeze=1)
s3=s()
s4=s2()

t3=s3.getTime()
t3.units='days since 2001'
x=vcs_legacy.init()
y=vcs_legacy.init()

b=x.createvector('new2')
b.datawc_y1=t0
b.datawc_y2=t1

x.plot(s,s2,b,bg=bg)
support.check_plot(x)
y.plot(s3,s4,b,bg=bg)
support.check_plot(x)
x.clear()
y.clear()

b.script('test.scr','w')

a = x.listelements('vector')
x.removeobject(b)
a2 = x.listelements('vector')
if a2==a:
    raise Exception,"Error elt not removed"
x.scriptrun('test.scr')
a3 = x.listelements('vector')
if a3!=a:
    raise Exception,"Error elt not loaded"
b=x.getvector('new2')
