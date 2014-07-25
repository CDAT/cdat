#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

var='tas'
import cdtime,cdms2,os,sys,vcs
from cdutil import times
import MV2

cdms2.setAutoBounds('on')

f   = cdms2.open(os.path.join(sys.prefix,'sample_data','tas_mo.nc'))
fsc = cdms2.open(os.path.join(sys.prefix,'sample_data','tas_mo_clim.nc'))

print "Step #0 : Reading data"
s=f(var,longitude=(0,360,'co'))

acok=fsc('climseas',longitude=(0,360,'co'))

print 'Test #1 : Test result'

ac=times.JAN.climatology(s)

assert(MV2.allclose(ac[0],acok[0]))

f.close()
fsc.close()

a=cdtime.comptime(1980)
b=cdtime.comptime(1980,5)

f = cdms2.open(os.path.join(sys.prefix,'sample_data','tas_6h.nc'))
s=f(var,time=(a,b,'co'),squeeze=1)

print "Test #2 : 6hourly AND get"
jans=times.JAN(s)

print "Test #3 : climatology 6h"
JFMA=times.Seasons('JFMA')
jfma=JFMA.climatology(s)


#Test reorder
print "Test #4 : time not first axis"
jfma=JFMA.climatology(s(order='x...'))
print "Test 4b: Result ok ?"
assert(jfma.getOrder()[0]=='x')



