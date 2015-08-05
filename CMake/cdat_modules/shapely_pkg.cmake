set( SHAPELY_MAJOR_SRC 1  )
set( SHAPELY_MINOR_SRC 5 )
set( SHAPELY_PATCH_SRC 9  )
set(SHAPELY_URL ${LLNL_URL})
set(SHAPELY_GZ
    Shapely-${SHAPELY_MAJOR_SRC}.${SHAPELY_MINOR_SRC}.${SHAPELY_PATCH_SRC}.tar.gz)
set(SHAPELY_MD5 b502824b154a49fbb8f33703d71557dd)
set(SHAPELY_SOURCE ${SHAPELY_URL}/${SHAPELY_GZ})

set (nm SHAPELY)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
add_cdat_package_dependent(Shapely "" "" OFF "CDAT_BUILD_GRAPHICS" OFF)
