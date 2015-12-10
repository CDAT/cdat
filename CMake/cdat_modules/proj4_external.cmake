set(proj4_source "${CMAKE_CURRENT_BINARY_DIR}/build/proj4")
set(proj4_install "${cdat_EXTERNALS}/proj4")
set(proj4_configure_args "")

ExternalProject_Add(proj4
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${proj4_source}
  INSTALL_DIR ${proj4_install}
  BUILD_IN_SOURCE 1
  URL ${PROJ4_SOURCE}
  URL_MD5 ${PROJ4_MD5}
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${proj4_configure_args} -P ${cdat_CMAKE_BINARY_DIR}/${configure_file}
  BUILD_COMMAND ${CMAKE_COMMAND} -Dmake=$(MAKE) -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake
  DEPENDS ${proj4_deps}
  ${ep_log_options}
)
if (DEFINED GIT_CMD_STR_PROJ4)
  unset(GIT_CMD_STR_PROJ4)
endif()
