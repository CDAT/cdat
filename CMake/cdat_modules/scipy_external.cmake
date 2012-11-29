# The Scipy external project 

set(SCIPY_binary "${CMAKE_CURRENT_BINARY_DIR}/SCIPY/")

# to configure scipy we run a cmake -P script
# the script will create a site.cfg file
# then run python setup.py config to verify setup
configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/SCIPY_configure_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/SCIPY_configure_step.cmake @ONLY)
# to build scipy we also run a cmake -P script.
# the script will set LD_LIBRARY_PATH so that 
# python can run after it is built on linux
configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/SCIPY_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/SCIPY_make_step.cmake @ONLY)

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/SCIPY_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/SCIPY_install_step.cmake @ONLY)

set(SCIPY_CONFIGURE_COMMAND ${CMAKE_COMMAND}
    -DCONFIG_TYPE=${CMAKE_CFG_INTDIR} -P ${cdat_CMAKE_BINARY_DIR}/SCIPY_configure_step.cmake)
set(SCIPY_BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/SCIPY_make_step.cmake)
set(SCIPY_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/SCIPY_install_step.cmake)

# create an external project to download scipy,
# and configure and build it
ExternalProject_Add(SCIPY
  URL ${SCIPY_URL}/${SCIPY_GZ}
  URL_MD5 ${SCIPY_MD5}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/SCIPY
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/SCIPY
  CONFIGURE_COMMAND ${SCIPY_CONFIGURE_COMMAND}
  BUILD_COMMAND ${SCIPY_BUILD_COMMAND}
  UPDATE_COMMAND ""
  INSTALL_COMMAND ${SCIPY_INSTALL_COMMAND}
  DEPENDS 
    ${SCIPY_deps}
  ${ep_log_options}
  )
