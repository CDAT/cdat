
set(geos_source "${CMAKE_CURRENT_BINARY_DIR}/build/geos-${GEOS_MAJOR}.${GEOS_MINOR}.${GEOS_PATCH}")
set(geos_install "${cdat_EXTERNALS}")

ExternalProject_Add(GEOS
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${geos_source}
  INSTALL_DIR ${geos_install}
  URL ${GEOS_URL}/${GEOS_BZ2}
  URL_MD5 ${GEOS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${GEOS_deps}
  ${ep_log_options}
)
