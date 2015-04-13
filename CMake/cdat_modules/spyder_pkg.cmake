set(SPYDER_VERSION 2.3.4)
set(SPYDER_URL ${LLNL_URL})
set(SPYDER_GZ spyder-${SPYDER_VERSION}.zip)
set(SPYDER_SOURCE ${SPYDER_URL}/${SPYDER_GZ})
set(SPYDER_MD5 e751f19b0c872cdfad6b1e3a69250f20)

add_cdat_package_dependent(spyder "" "" OFF "CDAT_BUILD_GUI" OFF)
