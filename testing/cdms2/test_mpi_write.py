import cdms2
import numpy


a=numpy.array([1,2,3,4,5,6,7,8.])

f=cdms2.open("test_mpi_write.nc","w")
f.write(a,id="test_mpi")
f.close()
