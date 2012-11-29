
set(udunits_source "${CMAKE_CURRENT_BINARY_DIR}/build/udunits2")
set(udunits_install "${cdat_EXTERNALS}")

ExternalProject_Add(udunits2
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${udunits_source}
  INSTALL_DIR ${udunits_install}
  URL ${UDUNITS2_URL}/${UDUNITS2_GZ}
  URL_MD5 ${UDUNITS2_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${udunits2_deps}
  ${ep_log_options}
)

