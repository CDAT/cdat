set(MPI_MAJOR 1)
set(MPI_MINOR 6)
set(MPI_URL ${LLNL_URL})
set(MPI_GZ openmpi-${MPI_MAJOR}.${MPI_MINOR}.tar.bz2)
set(MPI_MD5 dd6f5bd4b3cb14d93bbf530e50e46e60 )

add_cdat_package_dependent(MPI "" "Build MPI" ON "CDAT_BUILD_PARALLEL" OFF)

