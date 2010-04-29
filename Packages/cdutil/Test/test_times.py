#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

var='tas'
import cdtime,cdms2,os,sys,vcs
from cdutil import times
import MV2

cdms2.setAutoBounds('on')

f   = cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','tas_mo.nc'))
fsc = cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','tas_mo_clim.nc'))

print "Step #0 : Reading data"
s=f(var,longitude=(0,360,'co'))

acok=fsc('climseas',longitude=(0,360,'co'))

print 'Test #1 : Test result'

ac=times.JAN.climatology(s)

if not(MV2.allclose(ac[0],acok[0])) : raise 'Err answer seems to be wrong we Missing Value free dataset'

f.close()
fsc.close()

a=cdtime.comptime(1980)
b=cdtime.comptime(1980,5)

f = cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','tas_6h.nc'))
s=f(var,time=(a,b,'co'),squeeze=1)

print "Test #2 : 6hourly AND get"
jans=times.JAN(s)
try:
    jans=times.JAN(s)
except:
     raise 'Error computing januarys from 6h'

if '--extended' not in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   --extended option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()


print "Test #3 : climatology 6h"
JFMA=times.Seasons('JFMA')
try:
    jfma=JFMA.climatology(s)
except:
    raise 'Error computing climatological JFMA from 6h'


#Test reorder
print "Test #4 : time not first axis"
try:
    jfma=JFMA.climatology(s(order='x...'))
except:
    raise 'Error in (un)ordered slab, computing climatology'

print "Test 4b: Result ok ?"
if jfma.getOrder()[0]!='x' : raise "Error output order is  wrong"



