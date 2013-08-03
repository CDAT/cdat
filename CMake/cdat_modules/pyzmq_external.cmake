# The pyzmq project 

set(pyzmq_binary "${CMAKE_CURRENT_BINARY_DIR}/build/pyzmq")

# python can run after it is built on linux
configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/pyzmq_configure_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/pyzmq_configure_step.cmake @ONLY)

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/pyzmq_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/pyzmq_install_step.cmake @ONLY)

set(pyzmq_CONFIGURE_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pyzmq_configure_step.cmake)
set(pyzmq_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pyzmq_install_step.cmake)

set(pyzmq_source "${CMAKE_CURRENT_BINARY_DIR}/build/pyzmq")

# create an external project to download numpy,
# and configure and build it
ExternalProject_Add(pyzmq
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${pyzmq_source}
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/build/pyzmq
  URL ${PYZMQ_SOURCE}
  URL_MD5 ${PYZMQ_MD5}
  CONFIGURE_COMMAND ${pyzmq_CONFIGURE_COMMAND}
  BUILD_COMMAND ""
  UPDATE_COMMAND ""
  INSTALL_COMMAND ${pyzmq_INSTALL_COMMAND}
  DEPENDS
    ${pyzmq_deps}
  ${ep_log_options}
  )

# pyzmq
#

#ExternalProject_Add(pyzmq
#  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
#  SOURCE_DIR ${pyzmq_source}
#  URL ${PYZMQ_URL}/${PYZMQ_GZ}
#  URL_MD5 ${PYZMQ_MD5}
#  BUILD_IN_SOURCE 1
#  CONFIGURE_COMMAND ""
#  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
#  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
#  DEPENDS ${pyzmq_deps}
#  ${ep_log_options}
#  )
