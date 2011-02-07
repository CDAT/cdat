
set(Cairo_source "${CMAKE_CURRENT_BINARY_DIR}/Cairo")
set(Cairo_binary "${CMAKE_CURRENT_BINARY_DIR}/Cairo-build")
set(Cairo_install "${CMAKE_CURRENT_BINARY_DIR}/Cairo-install")

ExternalProject_Add(Cairo
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${Cairo_source}
  BINARY_DIR ${Cairo_build}
  INSTALL_DIR ${Cairo_install}
  URL ${CAIRO_URL}/${CAIRO_GZ}
  URL_MD5 ${CAIRO_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(Cairo_DIR "${Cairo_binary}" CACHE PATH "Cairo binary directory" FORCE)
mark_as_advanced(Cairo_DIR)
