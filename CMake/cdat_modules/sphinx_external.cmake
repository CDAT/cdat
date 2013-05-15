# create an external project to install Sphinx,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)
ExternalProject_Add(Sphinx
  DOWNLOAD_COMMAND ""
  WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${PIP_BINARY} install Sphinx==${SPHINX_VERSION}
  DEPENDS ${Sphinx_deps}
  ${ep_log_options}
  )

