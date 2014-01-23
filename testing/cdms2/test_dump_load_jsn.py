import cdms2,sys,numpy
f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt")

jsn = s.dumps()

s2=cdms2.createVariable(jsn,fromJSON=True)

assert(numpy.allclose(s2,s))


