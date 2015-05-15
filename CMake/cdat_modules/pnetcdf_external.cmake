set(pnetcdf_source "${CMAKE_CURRENT_BINARY_DIR}/build/pnetcdf")
set(pnetcdf_install "${cdat_EXTERNALS}")
set(pnetcdf_configure_args "--with-mpi=${cdat_EXTERNALS}")

ExternalProject_Add(PNETCDF
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${pnetcdf_source}
  INSTALL_DIR ${pnetcdf_install}
  URL ${PNETCDF_URL}/${PNETCDF_GZ}
  URL_MD5 ${PNETCDF_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -D CONFIGURE_ARGS=${pnetcdf_configure_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${PNETCDF_deps}
  ${ep_log_options}
)
