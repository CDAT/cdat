
set(libXSLT_source "${CMAKE_CURRENT_BINARY_DIR}/build/libXSLT")
set(libXSLT_install "${cdat_EXTERNALS}")

if(NOT LIBXML2_FOUND)
  set(libXSLT_configure_args --with-libxml-prefix=${libXSLT_install})
endif()

ExternalProject_Add(libXSLT
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${libXSLT_source}
  INSTALL_DIR ${libXSLT_install}
  URL ${XSLT_URL}/${XSLT_GZ}
  URL_MD5 ${XSLT_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCONFIGURE_ARGS=${libXSLT_configure_args} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${libXSLT_deps}
  ${ep_log_options}
)
