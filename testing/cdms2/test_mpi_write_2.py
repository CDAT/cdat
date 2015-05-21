import cdms2
import numpy
import mpi4py

sz = mpi4py.MPI.COMM_WORLD.Get_size()
rk = mpi4py.MPI.COMM_WORLD.Get_rank()

cdms2.setNetcdfClassicFlag(0)
cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

ntimes = 12000
a=numpy.ones((ntimes,64,128),dtype=numpy.float32)
a=cdms2.MV2.array(a)
a.id = "test_mpi"
print a.shape
f=cdms2.open("test_mpi_write_2.nc","w")
V=f.createVariableCopy(a)
print type(V)
#Ok now we are going to rewrite the time slice accross processors
for i in range(rk,ntimes,sz):
    V[i] = a[i].filled()*i
f.close()
