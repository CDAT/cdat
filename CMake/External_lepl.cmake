# create an external project to install lepl,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)

ExternalProject_Add(lepl
  DOWNLOAD_COMMAND ""
  WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${EASY_INSTALL_BINARY} lepl
  DEPENDS ${lepl_deps}
  ${EP_LOG_OPTIONS}
  )
