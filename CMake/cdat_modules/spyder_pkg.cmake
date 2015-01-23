set(SPYDER_VERSION 2.3.2)
set(SPYDER_URL ${LLNL_URL})
set(SPYDER_GZ spyder-${SPYDER_VERSION}.zip)
set(SPYDER_SOURCE ${SPYDER_URL}/${SPYDER_GZ})
set(SPYDER_MD5 fd3b4a51f7ecbf903f8788e0b8f215e0)

add_cdat_package_dependent(spyder "" "" OFF "CDAT_BUILD_GUI" OFF)
