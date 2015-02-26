set(LIBCDMS_VERSION 1.0.0)
set(LIBCDMS_URL ${LLNL_URL})
set(LIBCDMS_GZ libcdms-${LIBCDMS_VERSION}.tar.gz)
set(LIBCDMS_SOURCE ${LIBCDMS_URL}/${LIBCDMS_GZ})
set(LIBCDMS_MD5 ce71f54616f755d67fbbb6c81ca4fd62)

add_cdat_package(libcdms "" "" ON)
