import sys
import cdms2
import MV2
import cdutil
import numpy

f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt")
assert(numpy.allclose(s.missing_value,1.e20))
s.set_fill_value(1.e36)
s2=MV2.masked_greater(s,79.)
assert(numpy.allclose(s.missing_value,1.e36))
cdutil.times.setTimeBoundsMonthly(s2)
s3=cdutil.ANNUALCYCLE(s2)
assert(numpy.allclose(s.missing_value,1.e36))
s3=cdutil.ANNUALCYCLE.departures(s2)
assert(numpy.allclose(s.missing_value,1.e36))
s3=cdutil.ANNUALCYCLE.climatology(s2)
assert(numpy.allclose(s.missing_value,1.e36))
