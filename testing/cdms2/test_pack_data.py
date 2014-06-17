import cdms2
value = 0 
cdms2.setNetcdfShuffleFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateLevelFlag(value) ## where value is a integer between 0 and 9 included

import numpy,os

a = numpy.arange(20,dtype=numpy.float64)
b=numpy.arange(30,dtype=numpy.float64)
c=55.6*numpy.sin(a[:,numpy.newaxis]*b[numpy.newaxis,...])
#c=c.astype("f")

#for pack in [numpy.int8,numpy.int16,numpy.int32,numpy.int64,numpy.int]:
#  pass
def test_pack(c):
  for pack in [False,True]:
    #print "Testing pack:",pack

    f=cdms2.open("test_pack.nc","w")
    f.write(c,id='packed',pack=pack)
    f.close()

    f=cdms2.open("test_pack.nc")
    d=f("packed")
    f.close()

    assert(c.dtype==d.dtype)
    if pack in [True,numpy.int16]:
      atol = rtol = 1./pow(2,11)
  #  elif pack in [numpy.int,numpy.int64]:
  #    atol=rtol = 1./pow(2,63)
  #  elif pack == numpy.int32:
  #    atol=rtol = 1./pow(2,31)
  #  elif pack == numpy.int8:
  #    atol=rtol = 1./pow(2,7)
    elif pack is False:
      atol=rtol = 0.
    #print "M:",pack,c.max(),d.max()
    assert(numpy.ma.allclose(numpy.ma.maximum(c),numpy.ma.maximum(d),True,rtol,atol))
    #print "m:",pack,c.min(),d.min()
    assert(numpy.ma.allclose(numpy.ma.minimum(c),numpy.ma.minimum(d),True,rtol,atol))
    #print "D:",pack, (c-d).max(),(c-d).min(),((c-d)/c).max(),rtol
    assert(numpy.ma.allclose(c,d,True,rtol,atol))
    os.remove("test_pack.nc")

test_pack(c)
test_pack(numpy.ma.masked_greater(c,23))
test_pack(c.astype("f"))
test_pack(numpy.ma.masked_greater(c,23).astype("f"))

