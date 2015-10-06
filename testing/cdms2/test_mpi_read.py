import cdms2 
import mpi4py

sz = mpi4py.MPI.COMM_WORLD.Get_size()
rk = mpi4py.MPI.COMM_WORLD.Get_rank()

# Create a 2D array
# -----------------
lat = 181
lon = 361

f=cdms2.open("test_mpi_write_2.nc","r")
var=f("test_mpi_15")

V = var[rk+7:rk+10,:]
f.close()
print rk, V.shape

# Ask core number 8 to print its 3 lines
# --------------------------------------
#if rk == 7:
#   print "****"
#   print V[0,:]
##   print V[1,:]
#   print V[2,:]
