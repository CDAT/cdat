set(PYZMQ_VERSION 14.3.1)
set(PYZMQ_URL ${LLNL_URL})
set(PYZMQ_GZ pyzmq-${PYZMQ_VERSION}.tar.gz)
set(PYZMQ_MD5 7196b4a6fbf98022f17ffa924be3d68d)
set(PYZMQ_SOURCE ${PYZMQ_URL}/${PYZMQ_GZ})

add_cdat_package(pyzmq "" "" "")
