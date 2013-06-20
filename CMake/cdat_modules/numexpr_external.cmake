include(${cdat_CMAKE_BINARY_DIR}/cdat_common_environment.cmake)

if (INTERNET_ACCESS STREQUAL "ON") 
    set(EGG_GZ )
else ()
    set(EGG_GZ ${CDAT_PACKAGE_CACHE_DIR}/${NUMEXPR_GZ})
endif()

ExternalProject_Add(Numexpr
  DOWNLOAD_COMMAND ""
  WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${EGG_CMD} numexpr==${NUMEXPR_VERSION} ${EGG_GZ}
  DEPENDS ${Numexpr_deps}
  ${ep_log_options}
)


