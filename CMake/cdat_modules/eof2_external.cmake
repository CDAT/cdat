# EOF2
#
set(eof2_source "${CMAKE_CURRENT_BINARY_DIR}/build/eof2")

ExternalProject_Add(eof2
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${eof2_source}
  URL ${eof2_URL}/${eof2_GZ}
  URL_MD5 ${eof2_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${eof2_deps}
  ${ep_log_options}
)
