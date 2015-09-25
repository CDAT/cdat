# The X264 external project for ParaView
set(x264_source "${CMAKE_CURRENT_BINARY_DIR}/build/X264")
set(x264_install "${cdat_EXTERNALS}")
set(ENV{PATH} $ENV{PATH}:${cdat_EXTERNALS}/bin)

find_program(YASM_BIN "yasm")

if (NOT YASM_BIN)
  set(x264_conf_args --disable-asm^^--enable-shared)
else()
  set(x264_conf_args --enable-shared)
endif()

ExternalProject_Add(X264
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${x264_source}
  INSTALL_DIR ${x264_install}
  URL ${X264_URL}/${X264_GZ}
  URL_MD5 ${X264_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${x264_conf_args} -DCONFIGURE_SHELL=bash -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${X264_deps}
  ${ep_log_options}
  )

set(X264_INCLUDE_DIR ${x264_install}/include)
