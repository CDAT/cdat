#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

import cdms2,cdutil,os,sys

f=cdms2.open(os.path.join(sys.prefix,'sample_data','tas_mo.nc'))
s=f('tas')
tc=s.getTime().asComponentTime()

print tc[0],tc[-1]

cdutil.setTimeBoundsMonthly(s)
ref=cdutil.ANNUALCYCLE.climatology(s(time=('1980','1985','co')))
dep=cdutil.ANNUALCYCLE.departures(s)
ref=ref(order='y...')
dep=cdutil.ANNUALCYCLE.departures(s,ref=ref)
# testing that an ma in worng order would fail
try:
    dep=cdutil.ANNUALCYCLE.departures(s,ref=ref(order='t...').filled())
    raise RuntimeError( "Should have failed with ma passed as ref (not mv2)")
except:
  pass
