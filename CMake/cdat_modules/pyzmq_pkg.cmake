set(PYZMQ_VERSION 13.1.0)
set(PYZMQ_URL ${LLNL_URL})
set(PYZMQ_GZ pyzmq-${PYZMQ_VERSION}.tar.gz)
set(PYZMQ_SOURCE ${PYZMQ_URL}/${PYZMQ_GZ})

add_cdat_package(pyzmq "" "" "")
