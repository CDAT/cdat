set(R_deps ${readline_pkg})
if (CDAT_BUILD_PARALLEL)
  list(APPEND R_deps ${mpi_pkg})
endif()
