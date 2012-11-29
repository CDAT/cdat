# The FFMPEG external project for ParaView
set(ffmpeg_source "${CMAKE_CURRENT_BINARY_DIR}/build/FFMPEG")
set(ffmpeg_install "${cdat_EXTERNALS}")
set(ENV{PATH} $ENV{PATH}:${cdat_EXTERNALS}/bin)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/ffmpeg_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/ffmpeg_configure_step.cmake
    @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/ffmpeg_build_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/ffmpeg_build_step.cmake
    @ONLY)

ExternalProject_Add(FFMPEG
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ffmpeg_source}
  INSTALL_DIR ${ffmpeg_install}
  URL ${FFMPEG_URL}/${FFMPEG_GZ}
  URL_MD5 ${FFMPEG_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ffmpeg_configure_step.cmake
  BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ffmpeg_build_step.cmake
  DEPENDS ${FFMPEG_deps}
  ${ep_log_options}
  )

set(FFMPEG_INCLUDE_DIR ${ffmpeg_install}/include)
set(FFMPEG_avcodec_LIBRARY ${ffmpeg_install}/lib/libavcodec${_LINK_LIBRARY_SUFFIX})
set(FFMPEG_avformat_LIBRARY ${ffmpeg_install}/lib/libavformat${_LINK_LIBRARY_SUFFIX})
set(FFMPEG_avutil_LIBRARY ${ffmpeg_install}/lib/libavutil${_LINK_LIBRARY_SUFFIX})
set(FFMPEG_swscale_LIBRARY ${ffmpeg_install}/lib/libswscale${_LINK_LIBRARY_SUFFIX})
