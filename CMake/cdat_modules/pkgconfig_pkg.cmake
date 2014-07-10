set(PKG_MAJOR 0)
set(PKG_MINOR 9)
set(PKG_PATCH 0)
set(PKG_MAJOR_SRC 0)
set(PKG_MINOR_SRC 28)
set(PKG_PATCH_SRC 0)
set(PKG_VERSION ${PKG_MAJOR_SRC}.${PKG_MINOR_SRC}.${PKG_PATCH_SRC})
set(PKG_URL ${LLNL_URL})
set(PKG_GZ pkg-config-${PKG_MAJOR_SRC}.${PKG_MINOR_SRC}.tar.gz)
set(PKG_MD5 aa3c86e67551adc3ac865160e34a2a0d)
set(PKGCONFIG_VERSION ${PKG_VERSION})
set(PKGCONFIG_SOURCE ${PKG_URL}/${PKG_GZ})

add_cdat_package(pkgconfig "" "" OFF)

if(NOT CDAT_USE_SYSTEM_pkgconfig)
  set(cdat_PKG_CONFIG_EXECUTABLE ${cdat_EXTERNALS}/bin/pkg-config)
  set(ENV{PKG_CONFIG} "${cdat_PKG_CONFIG_EXECUTABLE}")
  set(ENV{PKG_CONFIG_PATH} "${cdat_EXTERNALS}/lib/pkgconfig:$ENV{PKG_CONFIG_PATH}")
  set(ENV{PKG_CONFIG} ${cdat_PKG_CONFIG_EXECUTABLE})
endif()

