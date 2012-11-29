# Cython
#
set(Cython_source "${CMAKE_CURRENT_BINARY_DIR}/build/Cython")

ExternalProject_Add(Cython
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${Cython_source}
  URL ${CYTHON_URL}/${CYTHON_GZ}
  URL_MD5 ${CYTHON_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${Cython_deps}
  ${ep_log_options}
)
