# configobj
#
set(configobj_source_dir "${CMAKE_CURRENT_BINARY_DIR}/build/configobj")

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/configobj_build_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/configobj_build_step.cmake"
  @ONLY
)

set(configobj_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/configobj_build_step.cmake)

ExternalProject_Add(configobj
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${configobj_source_dir}
  URL ${CONFIGOBJ_URL}/${CONFIGOBJ_GZ}
  URL_MD5 ${CONFIGOBJ_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${configobj_build_command}
  INSTALL_COMMAND ""
  DEPENDS ${configobj_deps}
  ${ep_log_options}
)
