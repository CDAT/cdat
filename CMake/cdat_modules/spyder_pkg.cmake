set(SPYDER_VERSION 2.3.0rc)
set(SPYDER_URL ${LLNL_URL})
set(SPYDER_GZ spyder-${SPYDER_VERSION}.zip)
set(SPYDER_SOURCE ${SPYDER_URL}/${SPYDER_GZ})
set(SPYDER_MD5 8bc5a8c5af2f4b03471a4b5b21c32691)


add_cdat_package_dependent(spyder "" "" ON "CDAT_BUILD_GUI" OFF)
