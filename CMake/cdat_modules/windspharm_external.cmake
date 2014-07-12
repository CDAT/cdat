# windspharm
#
set(windspharm_source "${CMAKE_CURRENT_BINARY_DIR}/build/windspharm")

ExternalProject_Add(windspharm
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${windspharm_source}
  URL ${windspharm_URL}/${windspharm_GZ}
  URL_MD5 ${windspharm_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${windspharm_deps}
  ${ep_log_options}
)
