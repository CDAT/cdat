
set(Cairo_source "${CMAKE_CURRENT_BINARY_DIR}/build/Cairo")
set(Cairo_install "${cdat_EXTERNALS}")
set(Cairo_conf_args --disable-static^^--enable-quartz=no^^--enable-win32=no^^--enable-skia=no^^--enable-os2=no^^--enable-beos=no^^--enable-drm=no^^--enable-gallium=no^^--enable-cogl=no)

ExternalProject_Add(Cairo
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${Cairo_source}
  INSTALL_DIR ${Cairo_install}
  URL ${CAIRO_URL}/${CAIRO_GZ}
  URL_MD5 ${CAIRO_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DSKIP_LDFLAGS=YES -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${Cairo_conf_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${Cairo_deps}
  ${ep_log_options}
)
