set(SPYDER_VERSION 2.2.5)
set(SPYDER_URL ${LLNL_URL})
set(SPYDER_GZ spyder-${SPYDER_VERSION}.zip)
set(SPYDER_SOURCE ${SPYDER_URL}/${SPYDER_GZ})
set(SPYDER_MD5 1c9aa650dae9f883616e917803f8a3be)


add_cdat_package_dependent(spyder "" "" ON "CDAT_BUILD_GUI" OFF)
