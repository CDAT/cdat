set(gdal_source "${CMAKE_CURRENT_BINARY_DIR}/build/gdal")
set(gdal_install "${cdat_EXTERNALS}")

ExternalProject_Add(gdal
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${gdal_source}
  INSTALL_DIR ${gdal_install}
  URL ${GDAL_URL}/${GDAL_GZ}
  URL_MD5 ${GDAL_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
  DEPENDS "${gdal_deps}"
  ${ep_log_options}
)
