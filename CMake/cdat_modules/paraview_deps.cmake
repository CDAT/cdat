set(ParaView_deps ${pkgconfig_pkg} ${python_pkg} ${hdf5_pkg} ${png_pkg} ${jpeg_pkg} ${libxml2_pkg} ${qt_pkg} ${freetype_pkg} ${netcdfplus_pkg} ${zlib_pkg} ${r_pkg})

if(CDAT_BUILD_PARALLEL)
  list(APPEND ParaView_deps "${mpi_pkg}")
endif()

