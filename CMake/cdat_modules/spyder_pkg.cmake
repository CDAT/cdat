set(SPYDER_VERSION 2.2.0)
set(SPYDER_URL ${LLNL_URL})
set(SPYDER_GZ spyder-${SPYDER_VERSION}.tar.gz)
set(SPYDER_SOURCE ${SPYDER_URL}/${SPYDER_GZ})
set(SPYDER_MD5 )

add_cdat_package_dependent(spyder "" "" ON "CDAT_BUILD_GUI" OFF)
