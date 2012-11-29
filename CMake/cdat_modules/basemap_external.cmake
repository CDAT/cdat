# The basemap external project 

set(basemap_binary "${CMAKE_CURRENT_BINARY_DIR}/basemap/")

#configure_file(
#  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/basemap_configure_step.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/basemap_configure_step.cmake @ONLY)
# to build we also run a cmake -P script.
# the script will set LD_LIBRARY_PATH so that 
# python can run after it is built on linux
configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/basemap_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/basemap_make_step.cmake @ONLY)

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/basemap_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/basemap_install_step.cmake @ONLY)

#set(basemap_CONFIGURE_COMMAND ${CMAKE_COMMAND}
#    -DCONFIG_TYPE=${CMAKE_CFG_INTDIR} -P ${cdat_CMAKE_BINARY_DIR}/basemap_configure_step.cmake)
set(basemap_BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/basemap_make_step.cmake)
set(basemap_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/basemap_install_step.cmake)

# create an external project to download numpy,
# and configure and build it
ExternalProject_Add(basemap
  URL ${basemap_URL}/${basemap_GZ}
  URL_MD5 ${basemap_MD5}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/basemap
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/basemap
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${basemap_BUILD_COMMAND}
  UPDATE_COMMAND ""
  INSTALL_COMMAND ${basemap_INSTALL_COMMAND}
  DEPENDS 
    ${basemap_deps}
  ${ep_log_options}
  )
