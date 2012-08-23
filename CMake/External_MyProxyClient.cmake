# create an external project to install MyProxyClient,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)

ExternalProject_Add(MyProxyClient
  DOWNLOAD_COMMAND ""
  WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${EASY_INSTALL_BINARY} MyProxyClient
  DEPENDS ${MyProxyClient_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
  )
