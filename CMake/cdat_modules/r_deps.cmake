set(R_deps ${readline_pkg})
if (CDAT_BUILD_PARALLEL)
  list(APPEND ${mpi_pkg})
endif()
