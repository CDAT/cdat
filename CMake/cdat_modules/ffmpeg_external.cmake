# The FFMPEG external project for ParaView
set(ffmpeg_source "${CMAKE_CURRENT_BINARY_DIR}/build/FFMPEG")
set(ffmpeg_install "${cdat_EXTERNALS}")
set(ENV{PATH} $ENV{PATH}:${cdat_EXTERNALS}/bin)

find_program(YASM_BIN "yasm")

if (NOT YASM_BIN)
  set(ffmpeg_conf_args --disable-yasm^^--enable-gpl^^--enable-libx264^^--extra-cxxflags=@ffmpeg_source@^^--enable-shared^^--enable-zlib)
else()
  set(ffmpeg_conf_args --enable-gpl^^--enable-libx264^^--extra-cxxflags=@ffmpeg_source@^^--enable-shared^^--enable-zlib)
endif()

ExternalProject_Add(FFMPEG
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ffmpeg_source}
  INSTALL_DIR ${ffmpeg_install}
  URL ${FFMPEG_URL}/${FFMPEG_GZ}
  URL_MD5 ${FFMPEG_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${ffmpeg_conf_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${FFMPEG_deps}
  ${ep_log_options}
  )

set(FFMPEG_INCLUDE_DIR ${ffmpeg_install}/include)
set(FFMPEG_avcodec_LIBRARY ${ffmpeg_install}/lib/libavcodec${_LINK_LIBRARY_SUFFIX})
set(FFMPEG_avformat_LIBRARY ${ffmpeg_install}/lib/libavformat${_LINK_LIBRARY_SUFFIX})
set(FFMPEG_avutil_LIBRARY ${ffmpeg_install}/lib/libavutil${_LINK_LIBRARY_SUFFIX})
set(FFMPEG_swscale_LIBRARY ${ffmpeg_install}/lib/libswscale${_LINK_LIBRARY_SUFFIX})
