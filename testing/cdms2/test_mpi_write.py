import cdms2
import numpy
cdms2.setNetcdfClassicFlag(0)
cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

a=numpy.array([1,2,3,4,5,6,7,8.])
a.shape=(1,2,2,2)
print a.shape
f=cdms2.open("test_mpi_write.nc","w")
f.write(a,id="test_mpi")
f.close()
