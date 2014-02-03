set(LXML_MAJOR_SRC 2)
set(LXML_MINOR_SRC 3)
set(LXML_PATCH_SRC 5)
set(LXML_URL ${LLNL_URL})
set(LXML_GZ lxml-${LXML_MAJOR_SRC}.${LXML_MINOR_SRC}.${LXML_PATCH_SRC}.tar.gz)
set(LXML_MD5 730bb63383528b65eaa099d64ce276cf)

set (nm LXML)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(LXML_SOURCE ${LXML_URL}/${LXML_GZ})

add_cdat_package_dependent(lxml "" "" ON "CDAT_BUILD_WO_ESGF" OFF)
