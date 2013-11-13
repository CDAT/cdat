import sys
import cdms2
import MV2
import cdutil

f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt")
print s.missing_value
s.set_fill_value(1.e36)
s2=MV2.masked_greater(s,79.)
print s2.missing_value
cdutil.times.setTimeBoundsMonthly(s2)
s3=cdutil.ANNUALCYCLE(s2)
print s3.missing_value
s3=cdutil.ANNUALCYCLE.departures(s2)
print s3.missing_value
s3=cdutil.ANNUALCYCLE.climatology(s2)
print s3.missing_value
