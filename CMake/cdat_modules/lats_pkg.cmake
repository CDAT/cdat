set(LATS_VERSION 1.0.0)
set(LATS_BRANCH master)
set(LATS_REPOSITORY ${GIT_PROTOCOL}github.com/UV-CDAT/lats.git )

set(GIT_CMD_STR_LATS GIT_REPOSITORY ${LATS_REPOSITORY})
set(GIT_TAG GIT_TAG "${LATS_BRANCH}")

if (CDAT_BUILD_PCMDI)
  add_cdat_package(lats "" "" ON)
endif()
