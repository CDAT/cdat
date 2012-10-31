set(CLAPACK_MAJOR 3)
set(CLAPACK_MINOR 2)
set(CLAPACK_PATCH 1)
set(CLAPACK_VERSION ${CLAPACK_MAJOR}.${CLAPACK_MINOR}.${CLAPACK_PATCH})
set(CLAPACK_URL http://www.netlib.org/clapack)
set(CLAPACK_GZ clapack-${CLAPACK_VERSION}-CMAKE.tgz)
set(CLAPACK_MD5 4fd18eb33f3ff8c5d65a7d43913d661b)

add_cdat_package(CLAPACK "" "" "")
set(CLAPACK_deps ${pkgconfig_pkg})

