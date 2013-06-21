set(SIP_configure_command ${PYTHON_EXECUTABLE} ${cdat_BINARY_DIR}/build/SIP/configure.py -b ${CMAKE_INSTALL_PREFIX}/bin -d ${PYTHON_SITE_PACKAGES} -e ${CMAKE_INSTALL_PREFIX}/include -v ${CMAKE_INSTALL_PREFIX}/share CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER})

ExternalProject_Add(SIP
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  URL ${SIP_URL}/${SIP_GZ}
  URL_MD5 ${SIP_MD5}
  SOURCE_DIR ${cdat_BINARY_DIR}/build/SIP
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${SIP_configure_command}
  DEPENDS ${SIP_deps}
  ${ep_log_options}
)

