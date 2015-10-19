set(nm mccabe)
string(TOUPPER ${nm} uc_nm)

set(${uc_nm}_MAJOR 0)
set(${uc_nm}_MINOR 3)
set(${uc_nm}_PATCH 1)
set(${uc_nm}_VERSION ${${uc_nm}_MAJOR}.${${uc_nm}_MINOR}.${${uc_nm}_PATCH})
set(${uc_nm}_URL ${LLNL_URL})
set(${uc_nm}_GZ ${nm}-${${uc_nm}_VERSION}.tar.gz)
set(${uc_nm}_MD5 9a1570c470ff5db678cc0c03d5c0c237 )

set(${uc_nm}_VERSION ${${uc_nm}_MAJOR}.${${uc_nm}_MINOR}.${${uc_nm}_PATCH})
set(${uc_nm}_SOURCE ${${uc_nm}_URL}/${${uc_nm}_GZ})

if(BUILD_TESTING)
  add_cdat_package(${nm} "" "" ON)
endif()
