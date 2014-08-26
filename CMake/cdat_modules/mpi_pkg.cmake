set(MPI_MAJOR 1)
set(MPI_MINOR 8)
set(MPI_PATCH 1)
set(MPI_URL ${LLNL_URL})
set(MPI_GZ openmpi-${MPI_MAJOR}.${MPI_MINOR}.${MPI_PATCH}.tar.gz)
set(MPI_MD5 9ca6fa1ef173cfb53419af9c9928c94b )
set(MPI_SOURCE ${MPI_URL}/${MPI_GZ})
set(MPI_VERSION ${MPI_MAJOR}.${MPI_MINOR}.${MPI_PATCH})

add_cdat_package_dependent(MPI "" "Build MPI" OFF "CDAT_BUILD_PARALLEL" OFF)

