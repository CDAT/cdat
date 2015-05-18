set(netcdf_source "${CMAKE_CURRENT_BINARY_DIR}/build/netcdf")
set(netcdf_install "${cdat_EXTERNALS}")
if (CDAT_BUILD_PARALLEL)
  message("[INFO] Building NetCDF with parallel ON")
  set(netcdf_configure_args "--enable-netcdf-4^^--enable-pnetcdf")
  set(configure_file "cdatmpi_configure_step.cmake")
else()
  set(netcdf_configure_args "--enable-netcdf-4")
  set(configure_file "cdat_configure_step.cmake")
endif()

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/netcdf_patch_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/netcdf_patch_step.cmake
  @ONLY)
  
set(netcdf_PATCH_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/netcdf_patch_step.cmake)

ExternalProject_Add(NetCDF
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${netcdf_source}
  INSTALL_DIR ${netcdf_install}
  URL ${NC4_URL}/${NC4_GZ}
  URL_MD5 ${NC4_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ${netcdf_PATCH_COMMAND}
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${netcdf_configure_args} -P ${cdat_CMAKE_BINARY_DIR}/${configure_file}
  BUILD_COMMAND ${CMAKE_COMMAND} -Dmake=$(MAKE) -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake
  DEPENDS ${NetCDF_deps}
  ${ep_log_options}
)

