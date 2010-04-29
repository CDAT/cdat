import sys,cdms2
print 'Test 17: Slicing and keeping dims ...',
f=cdms2.open(sys.prefix+'/sample_data/clt.nc')
s=f("clt")
s0=s[0]
assert s0.getAxis(0).isLatitude() == True
assert s0.getAxis(1).isLongitude() == True
print 'OK'
