import mpi4py
import cdms2

sz = mpi4py.MPI.COMM_WORLD.Get_size()
rk = mpi4py.MPI.COMM_WORLD.Get_rank()

# All flags are set to OFF for parallel writing
# ----------------------------------------------

cdms2.setNetcdfClassicFlag(0)
cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)
cdms2.setNetcdfUseParallelFlag(0)


if rk == 0:
    f=cdms2.open("test_mpi_write_2.nc")
    f.close
