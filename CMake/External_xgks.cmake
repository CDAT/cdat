
set(xgks_source "${CMAKE_CURRENT_BINARY_DIR}/xgks")
set(xgks_binary "${CMAKE_CURRENT_BINARY_DIR}/xgks-build")
set(xgks_install "${CMAKE_CURRENT_BINARY_DIR}/xgks-install")

ExternalProject_Add(xgks
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${xgks_source}
  BINARY_DIR ${xgks_build}
  INSTALL_DIR ${xgks_install}
  URL ${XGKS_URL}/${XGKS_GZ}
  URL_MD5 ${XGKS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(xgks_DIR "${xgks_binary}" CACHE PATH "xgks binary directory" FORCE)
mark_as_advanced(xgks_DIR)
