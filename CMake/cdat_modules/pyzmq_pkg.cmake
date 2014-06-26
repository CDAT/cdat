set(PYZMQ_VERSION 14.3.1)
set(PYZMQ_URL ${LLNL_URL})
set(PYZMQ_GZ pyzmq-${PYZMQ_VERSION}.tar.gz)
set(PYZMQ_MD5 b6323c14774ab5bd401112b259bf70be)
set(PYZMQ_SOURCE ${PYZMQ_URL}/${PYZMQ_GZ})
set(PYZMQ_MD5 b54a2209a4efed93ed00282d949db8d9)

add_cdat_package(pyzmq "" "" "")
