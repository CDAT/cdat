set(CLIGJ_MAJOR_SRC 0)
set(CLIGJ_MINOR_SRC 3)
set(CLIGJ_PATCH_SRC 0)

set(CLIGJ_VERSION ${CLIGJ_MAJOR_SRC}.${CLIGJ_MINOR_SRC}.${CLIGJ_PATCH_SRC})
set(CLIGJ_GZ cligj-${CLIGJ_VERSION}.tar.gz)
set(CLIGJ_SOURCE ${LLNL_URL}/${CLIGJ_GZ})
set(CLIGJ_MD5 cd135f171b4ef2c07ebd34731ccf09a5)

if (CDAT_BUILD_ALL)
  add_cdat_package(CLIGJ "" "" ON)
else()
  add_cdat_package(CLIGJ "" "" OFF)
endif()
