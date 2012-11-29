# create an external project to install lxml,
# and configure and build it
set(LXML_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/build/lxml)
set(LXML_BINARY_DIR ${LXML_SOURCE_DIR})

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/lxml_build_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/lxml_build_step.cmake @ONLY)
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/lxml_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/lxml_install_step.cmake @ONLY)

set(LXML_BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/lxml_build_step.cmake)
set(LXML_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/lxml_install_step.cmake)

ExternalProject_Add(lxml
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${LXML_SOURCE_DIR}
  URL ${LXML_URL}/${LXML_GZ}
  URL_MD5 ${LXML_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${LXML_BUILD_COMMAND}
  INSTALL_COMMAND ${LXML_INSTALL_COMMAND}
 # INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${lxml_deps}
  ${ep_log_options}
 )
