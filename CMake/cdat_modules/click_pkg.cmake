set(CLICK_MAJOR_SRC 4)
set(CLICK_MINOR_SRC 1)

set(CLICK_VERSION ${CLICK_MAJOR_SRC}.${CLICK_MINOR_SRC})
set(CLICK_GZ click-${CLICK_VERSION}.tar.gz)
set(CLICK_SOURCE ${LLNL_URL}/${CLICK_GZ})
set(CLICK_MD5 6a3fa88c738f2f775ec6de126feb99a4)

if (CDAT_BUILD_ALL)
  add_cdat_package(CLICK "" "" ON)
else()
  add_cdat_package(CLICK "" "" OFF)
endif()
