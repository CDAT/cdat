import cdms2, MV2
f = cdms2.open("test.nc","w")
s = MV2.ones((20,20))
s.id="test"
s.test_attribute = "some variable attribute"
f.test_attribute = "some file attribute"
f.write(s)
f.close()
f = cdms2.open("test.nc","r+")
delattr(f,'test_attribute')
s=f["test"]
del(s.test_attribute)
f.close()
f = cdms2.open("test.nc")
assert(hasattr(f,'test_attribute') is False)
s=f["test"]
assert(hasattr(s,'test_attribute') is False)
