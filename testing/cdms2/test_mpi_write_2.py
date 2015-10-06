import cdms2
import numpy
import mpi4py

sz = mpi4py.MPI.COMM_WORLD.Get_size()
rk = mpi4py.MPI.COMM_WORLD.Get_rank()

# All flags are set to OFF for parallel writing
# ----------------------------------------------

cdms2.setNetcdfClassicFlag(0)
cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

# Create a 2D array
# -----------------
lat = 181
lon = 361
a=numpy.ma.ones((lat,lon),dtype=numpy.float32)

# Create 1 degree grid of the world
# -----------------------------------
grid=cdms2.grid.createUniformGrid(-90, 181, 1, 0, 361, 1, order="yx")

Field=cdms2.MV2.array(a)

f=cdms2.open("test_mpi_write_2.nc","w")


# Create as many variable as you want
# and append the returned objec to a list
#
# Here we create "number of processor" variables
# Note that all porcessor MUST create all variables
# --------------------------------------------------
var=[]
for i in range(sz):
    Field.id = "test_mpi_"+str(i)
    myVar=f.createVariableCopy(Field,axes=grid.getAxisList(), grid=grid)
    var.append(myVar)

# Now rewrite the time slice accross processors
#
# Every processor run this section for all variables
# Each processor write a different slice of data
# the slice written for each processor depend of "rk"
#----------------------------------------------------
for k in range(sz):
   V=var[k]
   for j in range(rk,lat,sz):
         V[j] = a[j].filled()*rk
f.close()
