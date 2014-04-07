set(ncurses_source "${CMAKE_CURRENT_BINARY_DIR}/build/ncurses")
set(ncurses_install "${cdat_EXTERNALS}")
set(ncurses_conf_args)

ExternalProject_Add(ncurses
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ncurses_source}
  INSTALL_DIR ${ncurses_install}
  URL ${NCURSES_URL}/${NCURSES_GZ}
  URL_MD5 ${NCURSES_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCONFIGURE_ARGS=${ncurses_conf_args} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${ncurses_deps}
  ${ep_log_options}
)
