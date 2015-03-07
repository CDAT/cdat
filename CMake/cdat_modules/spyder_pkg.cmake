set(SPYDER_VERSION 2.3.3)
set(SPYDER_URL ${LLNL_URL})
set(SPYDER_GZ spyder-${SPYDER_VERSION}.zip)
set(SPYDER_SOURCE ${SPYDER_URL}/${SPYDER_GZ})
set(SPYDER_MD5 8442bbb0a74475ed3307d6e983ecd700)

add_cdat_package_dependent(spyder "" "" OFF "CDAT_BUILD_GUI" OFF)
