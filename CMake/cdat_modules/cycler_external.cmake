# Cycler

set(Cycler_source "${CMAKE_CURRENT_BINARY_DIR}/build/Cycler")

ExternalProject_Add(Cycler
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${Cycler_source}
  URL ${CYCLER_URL}/${CYCLER_GZ}
  URL_MD5 ${CYCLER_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install
  DEPENDS ${Cycler_deps}
  ${ep_log_options}
)
