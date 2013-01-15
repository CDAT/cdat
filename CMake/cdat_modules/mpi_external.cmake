
set(MPI_source "${CMAKE_CURRENT_BINARY_DIR}/build/MPI")
set(MPI_install "${cdat_EXTERNALS}")

ExternalProject_Add(MPI
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${MPI_source}
  INSTALL_DIR ${MPI_install}
  URL ${MPI_URL}/${MPI_GZ}
  URL_MD5 ${MPI_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR> --disable-vt
  DEPENDS ${MPI_deps}
  ${ep_log_options}
)
