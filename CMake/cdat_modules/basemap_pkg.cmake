set( basemap_MAJOR 1  )
set( basemap_MINOR 0  )
set( basemap_PATCH 5  )
set(basemap_URL ${LLNL_URL})
set(basemap_GZ basemap-${basemap_MAJOR}.${basemap_MINOR}.${basemap_PATCH}.tar.gz)
set(basemap_MD5 089260ea2b3eebb9d63e1783d0b15298 )
set(BASEMAP_VERSION ${basemap_MAJOR}.${basemap_MINOR}.${basemap_PATCH})
set(BASEMAP_SOURCE ${basemap_URL}/${basemap_GZ})

add_cdat_package_dependent(basemap "" "" ON "CDAT_BUILD_GRAPHICS" OFF)

