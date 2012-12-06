

set(readline_source "${CMAKE_CURRENT_BINARY_DIR}/build/readline")
set(readline_install "${cdat_EXTERNALS}")
set(readline_conf_args --with-curses)

ExternalProject_Add(readline
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${readline_source}
  INSTALL_DIR ${readline_install}
  URL ${READLINE_URL}/${READLINE_GZ}
  URL_MD5 ${READLINE_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ${CMAKE_COMMAND} -E copy_if_different ${cdat_external_patch_dir}/src/readline/shobj-conf ${readline_source}/support/shobj-conf
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCONFIGURE_ARGS=${readline_conf_args} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${readline_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

