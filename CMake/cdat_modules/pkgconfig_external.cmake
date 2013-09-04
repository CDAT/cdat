
set(pkgconfig_source "${CMAKE_CURRENT_BINARY_DIR}/build/pkgconfig")
set(pkgconfig_install "${cdat_EXTERNALS}")

ExternalProject_Add(pkgconfig
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  URL ${PKG_URL}/${PKG_GZ}
  URL_MD5 ${PKG_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  SOURCE_DIR ${pkgconfig_source}
  INSTALL_DIR ${pkgconfig_install}
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${pkgconfig_deps}
  ${ep_log_options}
)

