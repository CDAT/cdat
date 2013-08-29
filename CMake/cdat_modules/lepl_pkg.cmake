set(lepl_MAJOR_SRC 5)
set(lepl_MINOR_SRC 1)
set(lepl_PATCH_SRC 3)

set (nm lepl)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(LEPL_GZ LEPL-${LEPL_VERSION}.tar.gz)
set(LEPL_SOURCE ${LLNL_URL}/${LEPL_GZ})
set(LEPL_MD5 5f653984c57ad8efad828c5153660743 )

add_cdat_package_dependent(lepl "" "" ON "CDAT_BUILD_GUI" OFF)
