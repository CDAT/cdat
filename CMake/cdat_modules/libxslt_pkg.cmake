set(XSLT_MAJOR 1)
set(XSLT_MINOR 1)
set(XSLT_PATCH 22)
set(XSLT_MAJOR_SRC 1)
set(XSLT_MINOR_SRC 1)
set(XSLT_PATCH_SRC 26)
set(XSLT_URL ${LLNL_URL})
set(XSLT_GZ libxslt-${XSLT_MAJOR_SRC}.${XSLT_MINOR_SRC}.${XSLT_PATCH_SRC}.tar.gz)
set(XSLT_MD5 e61d0364a30146aaa3001296f853b2b9)

set (nm XSLT)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(LIBXSLT_VERSION ${XSLT_VERSION})
set(LIBXSLT_SOURCE ${XSLT_URL}/${XSLT_GZ})
set(LIBXSLT_MD5 ${XSLT_MD5})

add_cdat_package(libXSLT "" "Build xslt" "")

