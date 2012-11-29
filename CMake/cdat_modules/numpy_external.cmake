# The Numpy external project 

set(NUMPY_binary "${CMAKE_CURRENT_BINARY_DIR}/NUMPY/")

# to configure numpy we run a cmake -P script
# the script will create a site.cfg file
# then run python setup.py config to verify setup
configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/NUMPY_configure_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/NUMPY_configure_step.cmake @ONLY
)

# to build numpy we also run a cmake -P script.
# the script will set LD_LIBRARY_PATH so that 
# python can run after it is built on linux
configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/NUMPY_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/NUMPY_make_step.cmake @ONLY
)

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/NUMPY_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/NUMPY_install_step.cmake @ONLY
)

set(NUMPY_CONFIGURE_COMMAND ${CMAKE_COMMAND}
    -DCONFIG_TYPE=${CMAKE_CFG_INTDIR} -P ${cdat_CMAKE_BINARY_DIR}/NUMPY_configure_step.cmake)
set(NUMPY_BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/NUMPY_make_step.cmake)
set(NUMPY_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/NUMPY_install_step.cmake)

# create an external project to download numpy,
# and configure and build it
ExternalProject_Add(NUMPY
  URL ${NUMPY_URL}/${NUMPY_GZ}
  URL_MD5 ${NUMPY_MD5}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/NUMPY
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/NUMPY
  CONFIGURE_COMMAND ${NUMPY_CONFIGURE_COMMAND}
  BUILD_COMMAND ${NUMPY_BUILD_COMMAND}
  UPDATE_COMMAND ""
  INSTALL_COMMAND ${NUMPY_INSTALL_COMMAND}
  DEPENDS ${NUMPY_deps}
  ${ep_log_options}
)
