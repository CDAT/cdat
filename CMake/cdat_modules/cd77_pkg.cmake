set(cd77_VERSION 1.0.0)
set(cd77_BRANCH master)
set(cd77_REPOSITORY ${GIT_PROTOCOL}github.com/UV-CDAT/cd77.git )


set(GIT_CMD_STR_cd77 GIT_REPOSITORY ${cd77_REPOSITORY})
set(GIT_TAG_cd77 GIT_TAG "${cd77_BRANCH}")
set (nm cd77)
string(TOUPPER ${nm} uc_nm)

if (CDAT_BUILD_PCMDI)
  add_cdat_package(cd77 "" "" ON)
endif()
