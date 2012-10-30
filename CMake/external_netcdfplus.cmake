
set(netcdf_source "${CMAKE_CURRENT_BINARY_DIR}/build/netcdf-c++")
set(netcdf_install "${cdat_EXTERNALS}")
set(netcdf_configure_args "")

ExternalProject_Add(NetCDFPLUS
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${netcdf_source}
  INSTALL_DIR ${netcdf_install}
  URL ${NC4PLUS_URL}/${NC4PLUS_GZ}
  URL_MD5 ${NC4PLUS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${NetCDFPLUS_deps}
  ${ep_log_options}
)

