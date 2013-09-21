set(ParaView_deps ${pkgconfig_pkg} ${python_pkg} ${hdf5_pkg} ${png_pkg} ${jpeg_pkg} ${libxml2_pkg} ${freetype_pkg} ${netcdfplus_pkg} ${zlib_pkg})

if (NOT CDAT_BUILD_GUI)
  list(APPEND ParaView_deps ${qt_pkg})
endif()

if(CDAT_BUILD_PARALLEL)
  list(APPEND ParaView_deps "${mpi_pkg}")
endif()
