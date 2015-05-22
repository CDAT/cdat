import cdms2
import numpy
import mpi4py

sz = mpi4py.MPI.COMM_WORLD.Get_size()
rk = mpi4py.MPI.COMM_WORLD.Get_rank()

#cdms2.setNetcdfClassicFlag(0)
#cdms2.setNetcdf4Flag(1)
cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

a=numpy.array([1,2,3,4,5,6,7,8.])
a.shape=(1,2,2,2)
a=cdms2.MV2.array(a)
print a.shape
f=cdms2.open("test_mpi_write.nc","w")


#Ok now we are going to write variables, split accross or processors
nvars = 80
for i in range(rk,nvars,sz):
    #V=f.createVariableCopy(a,id="mpi_test_%i" % i)
    f.write(a*i,id="test_mpi_%i" % i)
f.close()
