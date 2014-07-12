# Windfield`
#
set(eofs_source "${CMAKE_CURRENT_BINARY_DIR}/build/eofs")

ExternalProject_Add(eofs
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${eofs_source}
  URL ${eofs_URL}/${eofs_GZ}
  URL_MD5 ${eofs_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${eofs_deps}
  ${ep_log_options}
)
