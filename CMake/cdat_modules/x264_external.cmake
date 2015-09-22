# The X264 external project for ParaView
set(x264_source "${CMAKE_CURRENT_BINARY_DIR}/build/X264")
set(x264_install "${cdat_EXTERNALS}")
set(ENV{PATH} $ENV{PATH}:${cdat_EXTERNALS}/bin)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/x264_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/x264_configure_step.cmake
    @ONLY)

set(x264_conf_args --enable-shared)

ExternalProject_Add(X264
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${x264_source}
  INSTALL_DIR ${x264_install}
  URL ${X264_URL}/${X264_GZ}
  URL_MD5 ${X264_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${x264_conf_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${X264_deps}
  ${ep_log_options}
  )

set(X264_INCLUDE_DIR ${x264_install}/include)
