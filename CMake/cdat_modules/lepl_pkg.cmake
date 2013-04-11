set(lepl_MAJOR_SRC 5)
set(lepl_MINOR_SRC 1)
set(lepl_PATCH_SRC 3)

set (nm lepl)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
add_cdat_package_dependent(lepl "" "" ON "CDAT_BUILD_GUI" OFF)
