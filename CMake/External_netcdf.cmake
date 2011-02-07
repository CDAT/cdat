
set(netcdf_source "${CMAKE_CURRENT_BINARY_DIR}/netcdf")
set(netcdf_binary "${CMAKE_CURRENT_BINARY_DIR}/netcdf-build")
set(netcdf_install "${CMAKE_CURRENT_BINARY_DIR}/netcdf-install")

ExternalProject_Add(netcdf
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${netcdf_source}
  BINARY_DIR ${netcdf_build}
  INSTALL_DIR ${netcdf_install}
  URL ${NC4_URL}/${NC4_GZ}
  URL_MD5 ${NC4_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(netcdf_DIR "${netcdf_binary}" CACHE PATH "netcdf binary directory" FORCE)
mark_as_advanced(netcdf_DIR)
