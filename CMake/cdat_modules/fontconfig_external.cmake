include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)

set(fontconfig_source "${CMAKE_CURRENT_BINARY_DIR}/build/fontconfig")
set(fontconfig_install "${cdat_EXTERNALS}")

ExternalProject_Add(fontconfig
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${fontconfig_source}
  INSTALL_DIR ${fontconfig_install}
  URL ${FTCFG_URL}/${FTCFG_GZ}
  URL_MD5 ${FTCFG_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=--disable-docs^^--enable-libxml2 -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${fontconfig_deps}
  ${ep_log_options}
)
