# vacumm
#
set(vacumm_source_dir "${CMAKE_CURRENT_BINARY_DIR}/build/vacumm")

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/vacumm_build_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/vacumm_build_step.cmake"
  @ONLY
)

set(vacumm_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/vacumm_build_step.cmake)

ExternalProject_Add(vacumm
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${vacumm_source_dir}
  URL ${VACUMM_URL}/${VACUMM_GZ}
  URL_MD5 ${VACUMM_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${vacumm_build_command}
  INSTALL_COMMAND ""
  DEPENDS ${vacumm_deps}
  ${ep_log_options}
)
