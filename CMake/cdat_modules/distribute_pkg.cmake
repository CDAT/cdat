set(DISTRIBUTE_MAJOR_SRC 0)
set(DISTRIBUTE_MINOR_SRC 6)
set(DISTRIBUTE_PATCH_SRC 45)

set (nm DISTRIBUTE)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(DISTRIBUTE_GZ distribute-${DISTRIBUTE_VERSION}.tar.gz)
set(DISTRIBUTE_SOURCE ${LLNL_URL}/${DISTRIBUTE_GZ})
set(DISTRIBUTE_MD5 8953f2c07e6700dabf2ec150129b8c31 )

add_cdat_package(distribute "" "" "")
