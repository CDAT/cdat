set(Ipython_genutils_source "${CMAKE_CURRENT_BINARY_DIR}/build/Ipython_genutils")

ExternalProject_Add(IPYTHON_GENUTILS
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${Ipython_genutils_source}
  URL ${IPYTHON_GENUTILS_URL}/${IPYTHON_GENUTILS_GZ}
  URL_MD5 ${IPYTHON_GENUTILS_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${Ipython_genutils_deps}
  ${ep_log_options}
)
