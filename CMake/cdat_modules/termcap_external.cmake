set(termcap_source "${CMAKE_CURRENT_BINARY_DIR}/build/termcap")
set(termcap_install "${cdat_EXTERNALS}")
set(termcap_conf_args)

ExternalProject_Add(termcap
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${termcap_source}
  INSTALL_DIR ${termcap_install}
  URL ${TCAP_URL}/${TCAP_GZ}
  URL_MD5 ${TCAP_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCONFIGURE_ARGS=${termcap_conf_args} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${termcap_deps}
  ${ep_log_options}
)
