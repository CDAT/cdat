
set(libXSLT_source "${CMAKE_CURRENT_BINARY_DIR}/libXSLT")
set(libXSLT_binary "${CMAKE_CURRENT_BINARY_DIR}/libXSLT-build")
set(libXSLT_install "${CMAKE_CURRENT_BINARY_DIR}/libXSLT-install")

ExternalProject_Add(libXSLT
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${libXSLT_source}
  BINARY_DIR ${libXSLT_build}
  INSTALL_DIR ${libXSLT_install}
  URL ${XSLT_URL}/${XSLT_GZ}
  URL_MD5 ${XSLT_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(libXSLT_DIR "${libXSLT_binary}" CACHE PATH "libXSLT binary directory" FORCE)
mark_as_advanced(libXSLT_DIR)
