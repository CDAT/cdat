# create an external project to install lxml,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)

set(lxml_source "${CMAKE_CURRENT_BINARY_DIR}/build/lxml")

ExternalProject_Add(lxml
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${lxml_source}
  URL ${LXML_URL}/${LXML_GZ}
  URL_MD5 ${LXML_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${lxml_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
  )

