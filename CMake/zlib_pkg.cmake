set(ZLIB_MAJOR_SRC 1)
set(ZLIB_MINOR_SRC 2)
#ZLIB_PATH_SRC and md5 is configured in CMakeLists.txt because on some RedHAt system we need to change it
#set(ZLIB_PATCH_SRC 3)
set(ZLIB_VERSION ${ZLIB_MAJOR_SRC}.${ZLIB_MINOR_SRC}.${ZLIB_PATCH_SRC})
set(ZLIB_URL ${LLNL_URL})
set(ZLIB_GZ zlib-${ZLIB_VERSION}.tar.gz)

add_cdat_package(zlib "" "" "")
set(zlib_deps ${pkgconfig_pkg})
