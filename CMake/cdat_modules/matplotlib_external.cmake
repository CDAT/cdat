# Matplotlib
#
set(matplotlib_source_dir "${CMAKE_CURRENT_BINARY_DIR}/build/Matplotlib")

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/matplotlib_patch_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/matplotlib_patch_step.cmake"
  @ONLY
)

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/matplotlib_build_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/matplotlib_build_step.cmake"
  @ONLY
)

set(matplotlib_patch_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/matplotlib_patch_step.cmake)
set(matplotlib_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/matplotlib_build_step.cmake)

ExternalProject_Add(Matplotlib
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${matplotlib_source_dir}
  URL ${MATPLOTLIB_URL}/${MATPLOTLIB_GZ}
  URL_MD5 ${MATPLOTLIB_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  PATCH_COMMAND ${matplotlib_patch_command}
  BUILD_COMMAND ${matplotlib_build_command}
  INSTALL_COMMAND ""
  DEPENDS ${Matplotlib_deps}
  ${ep_log_options}
)
