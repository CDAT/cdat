
set(Pmw_source "${CMAKE_CURRENT_BINARY_DIR}/build/Pmw")
set(Pmw_install "${cdat_EXTERNALS}")

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/pmw_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/pmw_make_step.cmake
  @ONLY)
  
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/pmw_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/pmw_install_step.cmake
  @ONLY)

set(Pmw_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pmw_make_step.cmake)
set(Pmw_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pmw_install_step.cmake)

ExternalProject_Add(Pmw
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${Pmw_source}
  INSTALL_DIR ${Pmw_install}
  URL ${PMW_URL}/${PMW_GZ}
  URL_MD5 ${PMW_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${Pmw_build_command}
  INSTALL_COMMAND ${Pmw_install_command}
  DEPENDS ${Pmw_deps}
  ${ep_log_options}
)

