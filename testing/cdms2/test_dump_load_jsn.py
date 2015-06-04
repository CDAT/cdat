import cdms2,sys,numpy,cdat_info
f=cdms2.open(cdat_info.get_sampledata_path()+"/clt.nc")
s=f("clt")

jsn = s.dumps()

s2=cdms2.createVariable(jsn,fromJSON=True)

assert(numpy.allclose(s2,s))


