set(SPHINX_MAJOR_SRC 1)
set(SPHINX_MINOR_SRC 2)
set(SPHINX_PATCH_SRC b1)

set (nm SPHINX)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}${${nm}_PATCH_SRC})
set(SPHINX_URL ${LLNL_URL})
set(SPHINX_GZ Sphinx-${SPHINX_VERSION}.tar.gz)
set(SPHINX_SOURCE ${SPHINX_URL}/${SPHINX_GZ})
set(SPHINX_MD5 67bea6df63be8e2a76ebedc76d8f71a3)

add_cdat_package_dependent(Sphinx "" "" ON "CDAT_BUILD_GUI" OFF)

