
set(HDF5_source "${CMAKE_CURRENT_BINARY_DIR}/build/HDF5")
set(HDF5_install "${cdat_EXTERNALS}")

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/hdf5_patch_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/hdf5_patch_step.cmake
  @ONLY)

# we disable HDF5 warnings because it has way too many of them.
ExternalProject_Add(HDF5
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${HDF5_source}
  INSTALL_DIR ${HDF5_install}
  URL ${HDF5_URL}/${HDF5_GZ}
  URL_MD5 ${HDF5_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ${CMAKE_COMMAND} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/hdf5_patch_step.cmake 
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DADDITIONAL_CFLAGS=-w -DADDITIONAL_CPPFPAGS=-w -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  LOG_BUILD 1
  DEPENDS ${HDF5_deps}
  ${ep_log_options}
)

if(WIN32)
  set(HDF5_INCLUDE_DIR ${HDF5_install}/include)
  set(HDF5_LIBRARY ${HDF5_install}/lib/hdf5dll${_LINK_LIBRARY_SUFFIX})
else()
  set(HDF5_INCLUDE_DIR ${HDF5_install}/include)
  set(HDF5_LIBRARY ${HDF5_install}/lib/libhdf5${_LINK_LIBRARY_SUFFIX})
endif()
