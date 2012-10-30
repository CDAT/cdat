add_cdat_package(pkgconfig "" "" "" "")

if(NOT CDAT_USE_SYSTEM_pkgconfig)
  set(cdat_PKG_CONFIG_EXECUTABLE ${cdat_EXTERNALS}/bin/pkg-config)
  set(ENV{PKG_CONFIG} "${cdat_PKG_CONFIG_EXECUTABLE}")
  set(ENV{PKG_CONFIG_PATH} "${cdat_EXTERNALS}/lib/pkgconfig:$ENV{PKG_CONFIG_PATH}")
  set(ENV{PKG_CONFIG} ${cdat_PKG_CONFIG_EXECUTABLE})
endif()

set(pkgconfig_deps ${wget_pkg})
