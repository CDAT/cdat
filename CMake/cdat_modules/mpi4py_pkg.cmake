set(MPI4PY_MAJOR 1)
set(MPI4PY_MINOR 3)
set(MPI4PY_VERSION ${MPI4PY_MAJOR}.${MPI4PY_MINOR})
set(MPI4PY_URL http://uv-cdat.llnl.gov/cdat/resources)
set(MPI4PY_GZ mpi4py-${MPI4PY_VERSION}.tar.gz)
set(MPI4PY_MD5 978472a1a71f3142c866c9463dec7103)
set(MPI4PY_SOURCE ${MPI4PY_URL}/${MPI4PY_GZ})


add_cdat_package_dependent(Mpi4py "" "Bulid Mpi4py" ON "CDAT_BUILD_PARALLEL" OFF)
