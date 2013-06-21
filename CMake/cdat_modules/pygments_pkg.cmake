set(pygments_MAJOR_SRC 1)
set(pygments_MINOR_SRC 6)
set(pygments_PATCH_SRC )

set (nm pygments)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC})
set(PYGMENTS_GZ Pygments-${PYGMENTS_VERSION}.tar.gz)
set(PYGMENTS_SOURCE ${LLNL_URL}/${PYGMENTS_GZ})
set(PYGMENTS_MD5 a18feedf6ffd0b0cc8c8b0fbdb2027b1  )

add_cdat_package_dependent(pygments "" "" ON "CDAT_BUILD_GUI" OFF)
