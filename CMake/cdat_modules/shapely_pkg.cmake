set( SHAPELY_MAJOR 1  )
set( SHAPELY_MINOR 2  )
set( SHAPELY_PATCH 14  )
set(SHAPELY_URL ${LLNL_URL})
set(SHAPELY_GZ Shapely-${SHAPELY_MAJOR}.${SHAPELY_MINOR}.${SHAPELY_PATCH}.tar.gz)
set(SHAPELY_MD5 be8efc68e83b3db086ec092a835ae4e5)

add_cdat_package(Shapely "" "" "")
