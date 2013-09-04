set( GDAL_MAJOR 1  )
set( GDAL_MINOR 9  )
set( GDAL_PATCH 1  )
set(GDAL_URL ${LLNL_URL})
set(GDAL_GZ gdal-${GDAL_MAJOR}.${GDAL_MINOR}.${GDAL_PATCH}.tar.gz)
set(GDAL_MD5 c5cf09b92dac1f5775db056e165b34f5)

set (nm GDAL)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR}.${${nm}_MINOR}.${${nm}_PATCH})
set(GDAL_SOURCE ${GDAL_URL}/${GDAL_GZ})

add_cdat_package(gdal "" "Build the Geospatial Data Abstraction Library (GDAL) and python osgeo module" OFF)
