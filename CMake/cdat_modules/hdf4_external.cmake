
set(HDF4_source "${CMAKE_CURRENT_BINARY_DIR}/HDF4")
set(HDF4_install "${cdat_EXTERNALS}")

if(NOT CMAKE_Fortran_COMPILER)
  set(hdf4_configure_args --disable-fortran)
else()
  set(hdf4_configure_args --enable-fortran)
endif()

ExternalProject_Add(HDF4
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${HDF4_source}
  INSTALL_DIR ${HDF4_install}
  URL ${HDF4_URL}/${HDF4_GZ}
  URL_MD5 ${HDF4_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${hdf4_configure_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${HDF4_deps}
)

set(HDF4_DIR "${HDF4_binary}" CACHE PATH "HDF4 binary directory" FORCE)
mark_as_advanced(HDF4_DIR)
