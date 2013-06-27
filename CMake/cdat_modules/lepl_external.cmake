# create an external project to install lepl,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)
if (NOT OFFLINE_BUILD)
    set(EGG_GZ lepl==${LEPL_VERSION})
else ()
    set(EGG_GZ ${CDAT_PACKAGE_CACHE_DIR}/${LEPL_GZ})
endif()

ExternalProject_Add(lepl
    DOWNLOAD_COMMAND ""
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${EGG_CMD} ${EGG_GZ}
  DEPENDS ${lepl_deps}
  ${ep_log_options}
  )
