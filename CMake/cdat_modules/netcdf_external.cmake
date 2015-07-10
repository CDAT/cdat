set(netcdf_source "${CMAKE_CURRENT_BINARY_DIR}/build/netcdf")
set(netcdf_binary "${CMAKE_CURRENT_BINARY_DIR}/build/netcdf-build")
set(netcdf_install "${cdat_EXTERNALS}")

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/netcdf_patch_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/netcdf_patch_step.cmake
  @ONLY)
  
set(netcdf_PATCH_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/netcdf_patch_step.cmake)

ExternalProject_Add(NetCDF
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${netcdf_source}
  BINARY_DIR ${netcdf_binary}
  INSTALL_DIR ${netcdf_install}
  URL ${NC4_URL}/${NC4_GZ}
  URL_MD5 ${NC4_MD5}
  CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX=${netcdf_install}
    #    -DHDF5_LIB=${cdat_EXTERNALS}/lib/libhdf5.so
    #-DHDF5_HL_LIB=${cdat_EXTERNALS}/lib/libhdf5_hl.so
    #-DHDF5_INCLUDE_DIR=${cdat_EXTERNALS}/include
    -DENABLE_NETCDF4:BOOL=ON
  PATCH_COMMAND ${netcdf_PATCH_COMMAND}
  DEPENDS ${NetCDF_deps}
  ${ep_log_options}
)

