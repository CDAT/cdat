
set(ESMP_source "${CMAKE_CURRENT_BINARY_DIR}/build/ESMP")

configure_file(${cdat_CMAKE_SOURCE_DIR}/ESMP_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/ESMP_install_step.cmake
  @ONLY)
set(ESMP_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ESMP_install_step.cmake)

ExternalProject_Add(ESMP
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR @ESMP_source@
  INSTALL_DIR @ESMP_install@
  URL ${ESMP_URL}/${ESMP_GZ}
#  URL_MD5 ${ESMP_MD5}
  URL_MD5 ""
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${ESMP_install_command}
  DEPENDS ${ESMP_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)
