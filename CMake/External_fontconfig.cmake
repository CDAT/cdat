
set(fontconfig_source "${CMAKE_CURRENT_BINARY_DIR}/fontconfig")
set(fontconfig_binary "${CMAKE_CURRENT_BINARY_DIR}/fontconfig-build")
set(fontconfig_install "${CMAKE_CURRENT_BINARY_DIR}/fontconfig-install")

ExternalProject_Add(fontconfig
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${fontconfig_source}
  BINARY_DIR ${fontconfig_build}
  INSTALL_DIR ${fontconfig_install}
  URL ${FTCFG_URL}/${FTCFG_GZ}
  URL_MD5 ${FTCFG_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(fontconfig_DIR "${fontconfig_binary}" CACHE PATH "fontconfig binary directory" FORCE)
mark_as_advanced(fontconfig_DIR)
