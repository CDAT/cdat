
set(netcdf_source "${CMAKE_CURRENT_BINARY_DIR}/build/netcdf")
set(netcdf_install "${CMAKE_CURRENT_BINARY_DIR}/Externals/NetCDF")
set(netcdf_configure_args "--enable-netcdf-4^^--with-hdf5=${CMAKE_CURRENT_BINARY_DIR}/Externals/HDF5")

ExternalProject_Add(netcdf
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${netcdf_source}
  INSTALL_DIR ${netcdf_install}
  URL ${NC4_URL}/${NC4_GZ}
  URL_MD5 ${NC4_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -D CONFIGURE_ARGS=${netcdf_configure_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${netcdf_DEPENDENCIES}
)

set(netcdf_DIR "${netcdf_binary}" CACHE PATH "netcdf binary directory" FORCE)
mark_as_advanced(netcdf_DIR)
