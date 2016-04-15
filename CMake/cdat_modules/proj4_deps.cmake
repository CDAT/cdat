set(proj4_deps ${pkgconfig_pkg})
if (CDAT_BUILD_PARALLEL)
      list(APPEND proj4_deps ${mpi_pkg})
endif()
