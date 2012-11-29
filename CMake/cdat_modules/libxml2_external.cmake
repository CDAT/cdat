
set(libXML2_source "${CMAKE_CURRENT_BINARY_DIR}/build/libXML2")
set(libXML2_install "${cdat_EXTERNALS}")

ExternalProject_Add(libXML2
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${libXML2_source}
  INSTALL_DIR ${libXML2_install}
  URL ${XML_URL}/${XML_GZ}
  URL_MD5 ${XML_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${libXML2_deps}
  ${ep_log_options}
)

