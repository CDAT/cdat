set(docutils_MAJOR_SRC 0)
set(docutils_MINOR_SRC 10)
set(docutils_PATCH_SRC )

set (nm docutils)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC})
set(DOCUTILS_GZ docutils-${DOCUTILS_VERSION}.tar.gz)
set(DOCUTILS_SOURCE ${LLNL_URL}/${DOCUTILS_GZ})
set(DOCUTILS_MD5 d8d4660c08302c791b2d71a155a2f4bc )

add_cdat_package_dependent(docutils "" "" ON "CDAT_BUILD_GUI" OFF)
