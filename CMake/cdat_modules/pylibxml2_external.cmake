# The python bindings to libxml2 external project 
set(PYLIBXML2_binary "${CMAKE_CURRENT_BINARY_DIR}/build/PYLIBXML2")

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/PYLIBXML2_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/PYLIBXML2_make_step.cmake @ONLY
)

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/PYLIBXML2_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/PYLIBXML2_install_step.cmake @ONLY
)

set(PYLIBXML2_BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/PYLIBXML2_make_step.cmake)
set(PYLIBXML2_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/PYLIBXML2_install_step.cmake)

# Create an external project to download libxml2python bindings,
# and configure and build it
ExternalProject_Add(PYLIBXML2
  URL ${PYLIBXML2_URL}/${PYLIBXML2_GZ}
  URL_MD5 ${PYLIBXML2_MD5}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${PYLIBXML2_binary}
  BINARY_DIR ${PYLIBXML2_binary}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYLIBXML2_BUILD_COMMAND}
  UPDATE_COMMAND ""
  INSTALL_COMMAND ${PYLIBXML2_INSTALL_COMMAND}
  DEPENDS ${PYLIBXML2_deps}
  ${ep_log_options}
)
