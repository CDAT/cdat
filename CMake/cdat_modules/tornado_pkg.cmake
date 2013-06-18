set(TORNADO_VERSION 3.1)
set(TORNADO_URL ${LLNL_URL})
set(TORNADO_GZ tornado-${TORNADO_VERSION}.tar.gz)
set(TORNADO_SOURCE ${TORNADO_URL}/${TORNADO_GZ})
set(TORNADO_MD5 )

add_cdat_package(tornado "" "" "")
