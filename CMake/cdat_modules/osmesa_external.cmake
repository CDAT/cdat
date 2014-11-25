set(osmesa_source "${CMAKE_CURRENT_BINARY_DIR}/build/osmesa")
set(osmesa_install "${cdat_EXTERNALS}")

set(osmesa_conf_args "--with-driver=osmesa")
set(osmesa_conf_args "${osmesa_conf_args}^^--disable-gallium")
set(osmesa_conf_args "${osmesa_conf_args}^^--disable-gallium-intel")
set(osmesa_conf_args "${osmesa_conf_args}^^--disable-egl")

ExternalProject_Add(OSMesa
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${osmesa_source}
  INSTALL_DIR ${osmesa_install}
  URL ${OSMESA_URL}/${OSMESA_GZ}
  URL_MD5 ${OSMESA_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND
    "${CMAKE_COMMAND}"
      "-DCONFIGURE_ARGS=${osmesa_conf_args}"
      "-DINSTALL_DIR=<INSTALL_DIR>"
      "-DWORKING_DIR=<SOURCE_DIR>"
      -P "${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake"
  DEPENDS ${osmesa_deps}
  ${ep_log_options}
)
