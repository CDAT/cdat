
set(esmp_source "${CMAKE_CURRENT_BINARY_DIR}/build/ESMP")
set(PYTHON_VER "python${PYTHON_MAJOR}.${PYTHON_MINOR}")
set(esmp_install "${CMAKE_INSTALL_PREFIX}/lib/${PYTHON_VER}/site-packages/")

configure_file(${cdat_CMAKE_SOURCE_DIR}/esmp_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/esmp_install_step.cmake
  @ONLY)
set(esmp_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/esmp_install_step.cmake)

ExternalProject_Add(ESMP
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR @esmp_source@
  INSTALL_DIR @esmp_install@
  URL ${ESMP_URL}/${ESMP_GZ}
  URL_MD5 ${ESMP_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${esmp_install_command}
  DEPENDS ${esmp_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)
