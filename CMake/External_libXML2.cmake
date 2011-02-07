
#  CONFIGURE_COMMAND <SOURCE_DIR>/configure
#                    --prefix=${prefix}
#                    --with-python=${CSE_PYTHON_HOME}

set(libXML2_source "${CMAKE_CURRENT_BINARY_DIR}/libXML2")
set(libXML2_binary "${CMAKE_CURRENT_BINARY_DIR}/libXML2-build")
set(libXML2_install "${CMAKE_CURRENT_BINARY_DIR}/libXML2-install")

ExternalProject_Add(libXML2
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${libXML2_source}
  BINARY_DIR ${libXML2_build}
  INSTALL_DIR ${libXML2_install}
  URL ${XML_URL}/${XML_GZ}
  URL_MD5 ${XML_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(libXML2_DIR "${libXML2_binary}" CACHE PATH "libXML2 binary directory" FORCE)
mark_as_advanced(libXML2_DIR)