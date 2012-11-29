
set(g2clib_source "${CMAKE_CURRENT_BINARY_DIR}/build/g2clib")
set(g2clib_install "${cdat_EXTERNALS}")

ExternalProject_Add(g2clib
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${g2clib_source}
  INSTALL_DIR ${g2clib_install}
  URL ${G2CLIB_URL}/${G2CLIB_GZ}
  URL_MD5 ${G2CLIB_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${CMAKE_COMMAND} -Dmake=$(MAKE) -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake
  DEPENDS ${g2clib_deps}
  ${ep_log_options}
)
