set( SHAPELY_MAJOR_SRC 1  )
set( SHAPELY_MINOR_SRC 2  )
set( SHAPELY_PATCH_SRC 14  )
set(SHAPELY_URL ${LLNL_URL})
set(SHAPELY_GZ
    Shapely-${SHAPELY_MAJOR_SRC}.${SHAPELY_MINOR_SRC}.${SHAPELY_PATCH_SRC}.tar.gz)
set(SHAPELY_MD5 be8efc68e83b3db086ec092a835ae4e5)
set(SHAPELY_SOURCE ${SHAPELY_URL}/${SHAPELY_GZ})

set (nm SHAPELY)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
add_cdat_package_dependent(Shapely "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
