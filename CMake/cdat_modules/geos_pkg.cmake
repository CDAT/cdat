set( GEOS_MAJOR 3  )
set( GEOS_MINOR 3  )
set( GEOS_PATCH 5  )
set(GEOS_URL ${LLNL_URL})
set(GEOS_BZ2 geos-${GEOS_MAJOR}.${GEOS_MINOR}.${GEOS_PATCH}.tar.bz2)
set(GEOS_MD5 2ba61afb7fe2c5ddf642d82d7b16e75b)

set (nm GEOS)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR}.${${nm}_MINOR}.${${nm}_PATCH})
set(GEOS_SOURCE ${GEOS_URL}/${GEOS_BZ2})

add_cdat_package_dependent(GEOS "" "" ON "CDAT_BUILD_WO_ESGF" OFF)
