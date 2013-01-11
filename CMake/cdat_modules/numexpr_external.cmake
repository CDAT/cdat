include(${cdat_CMAKE_BINARY_DIR}/cdat_common_environment.cmake)

ExternalProject_Add(Numexpr
  DOWNLOAD_COMMAND ""
  WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND env "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}" ${EASY_INSTALL_BINARY} Numexpr==${NUMEXPR_VERSION}
  DEPENDS ${Numexpr_deps}
  ${ep_log_options}
)


