set(TORNADO_VERSION 3.2.2)
set(TORNADO_URL ${LLNL_URL})
set(TORNADO_GZ tornado-${TORNADO_VERSION}.tar.gz)
set(TORNADO_SOURCE ${TORNADO_URL}/${TORNADO_GZ})
set(TORNADO_MD5 bf37082723ace27f508400d65cf111fe)

add_cdat_package(tornado "" "" "")
