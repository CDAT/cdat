
set(Pmw_source "${CMAKE_CURRENT_BINARY_DIR}/Pmw")
set(Pmw_binary "${CMAKE_CURRENT_BINARY_DIR}/Pmw-build")
set(Pmw_install "${CMAKE_CURRENT_BINARY_DIR}/Pmw-install")

ExternalProject_Add(Pmw
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${Pmw_source}
  BINARY_DIR ${Pmw_build}
  INSTALL_DIR ${Pmw_install}
  URL ${PMW_URL}/${PMW_GZ}
  URL_MD5 ${PMW_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(Pmw_DIR "${Pmw_binary}" CACHE PATH "Pmw binary directory" FORCE)
mark_as_advanced(Pmw_DIR)
