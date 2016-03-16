import cdms2, MV2
test_nm = 'CDMS_Test_del_attributes.nc'
f = cdms2.open(test_nm,"w")
s = MV2.ones((20,20))
s.id="test"
s.test_attribute = "some variable attribute"
f.test_attribute = "some file attribute"
f.write(s)
f.close()
f = cdms2.open(test_nm,"r+")
delattr(f,'test_attribute')
s=f["test"]
del(s.test_attribute)
f.close()
f = cdms2.open(test_nm)
assert(hasattr(f,'test_attribute') is False)
s=f["test"]
assert(hasattr(s,'test_attribute') is False)
