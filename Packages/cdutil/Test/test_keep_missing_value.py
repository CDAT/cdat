import sys
import cdms2
import MV2
f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt")
s.setMissing(1.e36)
print s.missing_value
s2=MV2.masked_greater(s,79.)
print s2.missing_value

