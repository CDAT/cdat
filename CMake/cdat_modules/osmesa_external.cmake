set(osmesa_source "${CMAKE_CURRENT_BINARY_DIR}/build/OSMesa")
set(osmesa_install "${cdat_EXTERNALS}")

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/osmesa_configure_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/osmesa_configure_step.cmake"
  @ONLY
)

ExternalProject_Add(OSMesa
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${osmesa_source}
  INSTALL_DIR ${osmesa_install}
  URL ${OSMESA_URL}/${OSMESA_GZ}
  URL_MD5 ${OSMESA_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/osmesa_configure_step.cmake
  DEPENDS ${OSMesa_deps}
)
