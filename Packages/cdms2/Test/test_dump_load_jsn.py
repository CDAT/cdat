import cdms2,sys
f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt")

jsn = s.dumps()

print type(jsn)

s2=cdms2.createVariable(jsn,fromJSON=True)
print s.min()
print s.max()
print s2.min()
print s2.max()

