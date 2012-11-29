
set(gifsicle_source "${CMAKE_CURRENT_BINARY_DIR}/build/gifsicle")
set(gifsicle_install "${cdat_EXTERNALS}")

ExternalProject_Add(gifsicle
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${gifsicle_source}
  INSTALL_DIR ${gifsicle_install}
  URL ${GIFSICLE_URL}/${GIFSICLE_GZ}
  URL_MD5 ${GIFSICLE_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${gifsicle_deps}
  ${ep_log_options}
)
