set(DISTRIBUTE_MAJOR_SRC 0)
set(DISTRIBUTE_MINOR_SRC 6)
set(DISTRIBUTE_PATCH_SRC 40)

set (nm DISTRIBUTE)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
#set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC})

add_cdat_package(distribute "" "" "")
