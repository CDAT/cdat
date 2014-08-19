set(OSMESA_MAJOR 10)
set(OSMESA_MINOR 2)
set(OSMESA_PATCH 5)
set(OSMESA_URL "ftp://ftp.freedesktop.org/pub/mesa/${OSMESA_MAJOR}.${OSMESA_MINOR}.${OSMESA_PATCH}/")
set(OSMESA_GZ "MesaLib-${OSMESA_MAJOR}.${OSMESA_MINOR}.${OSMESA_PATCH}.tar.gz")
set(OSMESA_MD5 877d3ee9085a9cdde6c049fcd40a11b2)

add_cdat_package_dependent(OSMesa "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
