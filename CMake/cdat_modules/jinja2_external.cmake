# create an external project to install jinja2,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)
if (NOT OFFLINE_BUILD)
    set(EGG_GZ jinja2==${JINJA2_VERSION})
else ()
    set(EGG_GZ ${CDAT_PACKAGE_CACHE_DIR}/${JINJA2_GZ})
endif()

ExternalProject_Add(jinja2
    DOWNLOAD_COMMAND ""
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${EGG_CMD} ${EGG_GZ}
  DEPENDS ${jinja2_deps}
  ${ep_log_options}
  )
