set(PYZMQ_VERSION 13.1.0)
set(PYZMQ_URL ${LLNL_URL})
set(PYZMQ_GZ pyzmq-${PYZMQ_VERSION}.tar.gz)
set(PYZMQ_SOURCE ${PYZMQ_URL}/${PYZMQ_GZ})
set(PYZMQ_MD5 b54a2209a4efed93ed00282d949db8d9)

add_cdat_package(pyzmq "" "" "")
