set(netcdf_source "${CMAKE_CURRENT_BINARY_DIR}/build/netcdf")
set(netcdf_binary "${CMAKE_CURRENT_BINARY_DIR}/build/netcdf-build")
set(netcdf_install "${cdat_EXTERNALS}")
set(netcdf_configure_args "--enable-netcdf-4")

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/netcdf_patch_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/netcdf_patch_step.cmake
  @ONLY)
  
set(netcdf_PATCH_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/netcdf_patch_step.cmake)

list(APPEND netcdf_build_args
  -DHDF5_DIR:PATH=${cdat_EXTERNALS}
  )
ExternalProject_Add(NetCDF
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${netcdf_source}
  INSTALL_DIR ${netcdf_install}
  BINARY_DIR ${netcdf_binary}
  URL ${NC4_URL}/${NC4_GZ}
  URL_MD5 ${NC4_MD5}
  #${NC4_GIT_CMD}
  #${NC4_GIT_TAG}
  CMAKE_CACHE_ARGS
       ${netcdf_build_args}
  CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  PATCH_COMMAND ${netcdf_PATCH_COMMAND}
  DEPENDS ${NetCDF_deps}
  ${ep_log_options}
)

