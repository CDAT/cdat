set(CMOR_VERSION 2.9.2)
set(CMOR_BRANCH master)
set(CMOR_REPOSITORY ${GIT_PROTOCOL}github.com/PCMDI/cmor.git )

set(GIT_CMD_STR_CMOR GIT_REPOSITORY ${CMOR_REPOSITORY})
set(GIT_TAG GIT_TAG "${CMOR_BRANCH}")
set (nm CMOR)
string(TOUPPER ${nm} uc_nm)

if (CDAT_BUILD_ALL)
  add_cdat_package(CMOR "" "" ON)
else()
  add_cdat_package_dependent(CMOR "" "" ON "CDAT_BUILD_CMOR" OFF)
endif()
