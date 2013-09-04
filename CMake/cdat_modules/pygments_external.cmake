# create an external project to install pygments,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)
if (NOT OFFLINE_BUILD)
    set(EGG_GZ pygments==${PYGMENTS_VERSION})
else ()
    set(EGG_GZ ${CDAT_PACKAGE_CACHE_DIR}/${PYGMENTS_GZ})
endif()

ExternalProject_Add(pygments
    DOWNLOAD_COMMAND ""
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${EGG_CMD} ${EGG_GZ}
  DEPENDS ${pygments_deps}
  ${ep_log_options}
  )
