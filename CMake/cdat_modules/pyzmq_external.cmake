# create an external project to install pyzmq,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)

ExternalProject_Add(pyzmq
  DOWNLOAD_COMMAND ""
  WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${PIP_BINARY} install pyzmq==${PYZMQ_VERSION}
  DEPENDS ${pyzmq_deps}
  ${ep_log_options}
  )
