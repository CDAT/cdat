# Windfield`
#
set(windfield_source "${CMAKE_CURRENT_BINARY_DIR}/build/windfield")

ExternalProject_Add(windfield
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${windfield_source}
  URL ${windfield_URL}/${windfield_GZ}
  URL_MD5 ${windfield_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${windfield_deps}
  ${ep_log_options}
)
