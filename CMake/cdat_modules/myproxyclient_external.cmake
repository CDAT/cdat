# create an external project to install MyProxyClient,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)
if (NOT OFFLINE_BUILD)
    set(EGG_GZ MyProxyClient==${MYPROXYCLIENT_VERSION})
else ()
    set(EGG_GZ ${CDAT_PACKAGE_CACHE_DIR}/${MYPROXYCLIENT_GZ})
endif()

ExternalProject_Add(MyProxyClient
  DOWNLOAD_COMMAND ""
  WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${EGG_CMD} ${EGG_GZ}
  DEPENDS ${MyProxyClient_deps}
  ${ep_log_options}
  )
