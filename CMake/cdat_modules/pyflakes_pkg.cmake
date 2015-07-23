set(nm pyflakes)
string(TOUPPER ${nm} uc_nm)

set(${uc_nm}_MAJOR 0)
set(${uc_nm}_MINOR 8)
set(${uc_nm}_PATCH 1)
set(${uc_nm}_VERSION ${${uc_nm}_MAJOR}.${${uc_nm}_MINOR}.${${uc_nm}_PATCH})
set(${uc_nm}_URL ${LLNL_URL})
set(${uc_nm}_GZ ${nm}-${${uc_nm}_VERSION}.tar.gz)
set(${uc_nm}_MD5 905fe91ad14b912807e8fdc2ac2e2c23 )

set(${uc_nm}_VERSION ${${uc_nm}_MAJOR}.${${uc_nm}_MINOR}.${${uc_nm}_PATCH})
set(${uc_nm}_SOURCE ${${uc_nm}_URL}/${${uc_nm}_GZ})

if(BUILD_TESTING)
  add_cdat_package(${nm} "" "" ON)
endif()
