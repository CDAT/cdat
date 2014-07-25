#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

import cdms2,cdutil,sys,os,numpy
cdms2.setAutoBounds('on')

f = cdms2.open(os.path.join(sys.prefix,'sample_data','th_yr.nc'))

th=f('th',time=slice(-3,None,1))
t=th.getTime()
cdutil.setTimeBoundsYearly(t)

assert(th.shape==(3,64,128))
assert(numpy.equal(th.getTime().getBounds()[0],[1997.,1998.]).all())
dep=cdutil.YEAR.departures(th,statusbar=None)
assert(dep.shape==(3, 64, 128))
