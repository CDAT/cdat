import sys,cdms2,cdat_info
print 'Test 17: Slicing and keeping dims ...',
f=cdms2.open(cdat_info.get_prefix()+'/sample_data/clt.nc')
s=f("clt")
s0=s[0]
assert s0.getAxis(0).isLatitude() == True
assert s0.getAxis(1).isLongitude() == True
print 'OK'
