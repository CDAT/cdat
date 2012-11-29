
set(ghostscript_source "${CMAKE_CURRENT_BINARY_DIR}/build/ghostscript")
set(ghostscript_install "${cdat_EXTERNALS}")

set(ghostscripts_args "--with-drivers=PS,BMP --disable-cups")

ExternalProject_Add(ghostscript
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ghostscript_source}
  INSTALL_DIR ${ghostscript_install}
  URL ${GS_URL}/${GS_GZ}
  URL_MD5 ${GS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${ghostscripts_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  BUILD_COMMAND ${CMAKE_COMMAND} -Dmake=$(MAKE) -DBUILD_ARGS=${ghostscript_source} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  DEPENDS ${ghostscript_deps}
  ${ep_log_options}
)
