# seawater
#
set(seawater_source_dir "${CMAKE_CURRENT_BINARY_DIR}/build/seawater")

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/seawater_build_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/seawater_build_step.cmake"
  @ONLY
)

set(seawater_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/seawater_build_step.cmake)

ExternalProject_Add(seawater
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${seawater_source_dir}
  URL ${SEAWATER_URL}/${SEAWATER_GZ}
  URL_MD5 ${SEAWATER_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${seawater_build_command}
  INSTALL_COMMAND ""
  DEPENDS ${seawater_deps}
  ${ep_log_options}
)
