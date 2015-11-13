set(curses_source "${CMAKE_CURRENT_BINARY_DIR}/build/curses")
set(curses_install "${cdat_EXTERNALS}")
set(curses_conf_args)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/curses_patch_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/curses_patch_step.cmake
  @ONLY)

set(curses_PATCH_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/curses_patch_step.cmake)

ExternalProject_Add(Curses
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${curses_source}
  INSTALL_DIR ${curses_install}
  URL ${CURSES_URL}/${CURSES_GZ}
  URL_MD5 ${CURSES_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ${curses_PATCH_COMMAND}
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCONFIGURE_ARGS=${curses_conf_args} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${curses_deps}
  ${ep_log_options}
)
