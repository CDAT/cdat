set(NetCDF_deps ${pkgconfig_pkg} ${hdf5_pkg} ${curl_pkg} ${zlib_pkg} ${jpeg_pkg} )
if (CDAT_BUILD_PARALLEL)
  list(APPEND NetCDF_deps ${mpi_pkg} ${pnetcdf_pkg})
endif()
