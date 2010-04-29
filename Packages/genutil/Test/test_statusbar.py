# Adapted for numpy/ma/cdms2 by convertcdms.py
from genutil import statusbar
import os,sys,cdutil
n=100

try:
    prev=0
    for i in range(n):
        prev=statusbar(i,n-1,prev=prev)
except:
    raise "Error simple statusbar died...."
print
try:
    prev=0
    for i in range(n):
        prev=statusbar(i,float(n-1),prev=prev,tk=1)
except:
    raise "Error simple tk statusbar died...."


import cdutil,cdms2 as cdms
f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('clt')
cdutil.setTimeBoundsMonthly(s)
try:
    ac=cdutil.ANNUALCYCLE( s , statusbar=1 )
except:
    raise "Error statusbar of extractions died (not tk)...."

try:
    ac=cdutil.ANNUALCYCLE.climatology( s , statusbar=1 )
except:
    raise "Error statusbar of climatologies died (not tk)...."

try:
    dep=cdutil.ANNUALCYCLE.departures( s , statusbar=1 )
except:
    raise "Error statusbar of departures died (not tk)...."

statusbar.tk__=1
try:
    ac=cdutil.ANNUALCYCLE( s , statusbar=1 )
except:
    raise "Error statusbar of extractions died with tk...."

try:
    ac=cdutil.ANNUALCYCLE.climatology( s , statusbar=1 )
except:
    raise "Error statusbar of climatologies died with tk...."

try:
    dep=cdutil.ANNUALCYCLE.departures( s , statusbar=1 )
except:
    raise "Error statusbar of departures died with tk...."

