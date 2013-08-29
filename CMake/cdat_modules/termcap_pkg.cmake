set(TCAP_MAJOR_SRC 1)
set(TCAP_MINOR_SRC 3)
set(TCAP_PATCH_SRC 1)
set(TCAP_URL ${LLNL_URL})
set(TCAP_GZ termcap-${TCAP_MAJOR_SRC}.${TCAP_MINOR_SRC}.${TCAP_PATCH_SRC}.tar.gz)
set(TCAP_MD5 ffe6f86e63a3a29fa53ac645faaabdfa)
set(TERMCAP_SOURCE ${TCAP_URL}/${TCAP_GZ})
set(TERMCAP_MD5 ${TCAP_MD5})

set (nm TCAP)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(TERMCAP_VERSION ${TCAP_VERSION})

add_cdat_package(termcap "" "" "")

