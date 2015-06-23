set(nm flake8)
string(TOUPPER ${nm} uc_nm)

set(${uc_nm}_MAJOR 2)
set(${uc_nm}_MINOR 4)
set(${uc_nm}_PATCH 1)
set(${uc_nm}_VERSION ${${uc_nm}_MAJOR}.${${uc_nm}_MINOR}.${${uc_nm}_PATCH})
set(${uc_nm}_URL http://pypi.python.org/packages/source/f/flake8)
set(${uc_nm}_GZ ${nm}-${${uc_nm}_VERSION}.tar.gz)
set(${uc_nm}_MD5 ed45d3db81a3b7c88bd63c6e37ca1d65)

set(${uc_nm}_VERSION ${${uc_nm}_MAJOR}.${${uc_nm}_MINOR}.${${uc_nm}_PATCH})
set(${uc_nm}_SOURCE ${${uc_nm}_URL}/${${uc_nm}_GZ})

if(BUILD_TESTING)
  add_cdat_package(${nm} "" "" ON)
endif()
