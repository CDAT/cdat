set(LAPACK_MAJOR_SRC 3)
set(LAPACK_MINOR_SRC 4)
set(LAPACK_PATCH_SRC 2)

set(LAPACK_URL ${LLNL_URL})
set(LAPACK_GZ lapack-${LAPACK_MAJOR_SRC}.${LAPACK_MINOR_SRC}.${LAPACK_PATCH_SRC}.tgz)
set(LAPACK_MD5 61bf1a8a4469d4bdb7604f5897179478 )

set (nm LAPACK)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})

#Removing apple exclusion for now
set(LAPACK_SOURCE ${LAPACK_URL}/${LAPACK_GZ})

if(NOT APPLE)
  if(CMAKE_Fortran_COMPILER)
    add_cdat_package(LAPACK "" "" "")
  endif()
endif()
