
set(xslt_source "${CMAKE_CURRENT_BINARY_DIR}/build/xslt")
set(xslt_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

set(xslt_configure_args --with-libxml-prefix=${xslt_install})

ExternalProject_Add(xslt
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${xslt_source}
  INSTALL_DIR ${xslt_install}
  URL ${XSLT_URL}/${XSLT_GZ}
  URL_MD5 ${XSLT_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCONFIGURE_ARGS=${xslt_configure_args} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${xslt_DEPENDENCIES}
)

