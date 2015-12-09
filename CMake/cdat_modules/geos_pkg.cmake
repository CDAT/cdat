set( GEOS_MAJOR 3  )
set( GEOS_MINOR 5  )
set( GEOS_PATCH 0  )
set(GEOS_URL ${LLNL_URL})
set(GEOS_BZ2 geos-${GEOS_MAJOR}.${GEOS_MINOR}.${GEOS_PATCH}.tar.bz2)
set(GEOS_MD5 136842690be7f504fba46b3c539438dd)

set (nm GEOS)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR}.${${nm}_MINOR}.${${nm}_PATCH})
set(GEOS_SOURCE ${GEOS_URL}/${GEOS_BZ2})

add_cdat_package_dependent(GEOS "" "" OFF "CDAT_BUILD_GRAPHICS" OFF)
