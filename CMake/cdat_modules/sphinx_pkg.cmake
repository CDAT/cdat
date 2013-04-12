set(SPHINX_MAJOR_SRC 1)
set(SPHINX_MINOR_SRC 1)
set(SPHINX_PATCH_SRC 3)

set (nm SPHINX)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
add_cdat_package_dependent(Sphinx "" "" ON "CDAT_BUILD_WO_ESGF" OFF)

