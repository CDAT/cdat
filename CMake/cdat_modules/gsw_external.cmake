# gsw (Gibbs Seawater)
#
set(gsw_source_dir "${CMAKE_CURRENT_BINARY_DIR}/build/gsw")

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/gsw_build_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/gsw_build_step.cmake"
  @ONLY
)

set(gsw_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/gsw_build_step.cmake)

ExternalProject_Add(gsw
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${gsw_source_dir}
  URL ${GSW_URL}/${GSW_GZ}
  URL_MD5 ${GSW_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${gsw_build_command}
  INSTALL_COMMAND ""
  DEPENDS ${gsw_deps}
  ${ep_log_options}
)
