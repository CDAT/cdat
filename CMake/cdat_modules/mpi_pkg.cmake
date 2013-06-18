set(MPI_MAJOR 1)
set(MPI_MINOR 6)
set(MPI_PATCH 4)
set(MPI_URL ${LLNL_URL})
set(MPI_GZ openmpi-${MPI_MAJOR}.${MPI_MINOR}.${MPI_PATCH}.tar.gz)
set(MPI_MD5 70aa9b6271d904c6b337ca326e6613d1 )
set(MPI_SOURCE ${MPI_URL}/${MPI_GZ})
set(MPI_VERSION ${MPI_MAJOR}.${MPI_MINOR}.${MPI_PATCH})

add_cdat_package_dependent(MPI "" "Build MPI" ON "CDAT_BUILD_PARALLEL" OFF)

