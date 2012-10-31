set( GDAL_MAJOR 1  )
set( GDAL_MINOR 9  )
set( GDAL_PATCH 1  )
set(GDAL_URL ${LLNL_URL})
set(GDAL_GZ gdal-${GDAL_MAJOR}.${GDAL_MINOR}.${GDAL_PATCH}.tar.gz)
set(GDAL_MD5 c5cf09b92dac1f5775db056e165b34f5)

add_cdat_package(gdal "" "" "" "")
