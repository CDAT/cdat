# Adapted for numpy/ma/cdms2 by convertcdms.py
import sys,cdms2 as cdms,vcs_legacy,cdtime,support,os
bg=support.bg

t0=cdtime.comptime(1987,8)
t1=cdtime.comptime(1988,12)
f=cdms.open(os.path.join(vcs.sample_data,'ta_ncep_87-6-88-4.nc'))

s=f('ta',latitude=slice(5,6),level=slice(0,1),squeeze=1)
mn,mx=vcs_legacy.minmax(s)
s=s/mx
s2=s()
t2=s2.getTime()
t2.units='months since 1949-5'
x=vcs_legacy.init()
y=vcs_legacy.init()



b=x.createoutfill('new2')
b.datawc_y1=t0
b.datawc_y2=t1

x.plot(s,b,bg=bg)
support.check_plot(x)
y.plot(s2,b,bg=bg)
support.check_plot(y)
x.clear()
y.clear()

b.script('test.scr','w')

a= x.listelements('outfill')
x.removeobject(b)
a2 =  x.listelements('outfill')
if a2==a:
  raise Exception,"Error, object not removed"
x.scriptrun('test.scr')
a3 = x.listelements('outfill')
if a3!=a:
  raise Exception,"Error, object not loaded from script"
b=x.getoutfill('new2')
