# The Mpi4py project 

set(mpi4py_binary "${CMAKE_CURRENT_BINARY_DIR}/build/Mpi4py")

# python can run after it is built on linux
configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/mpi4py_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/mpi4py_make_step.cmake @ONLY)

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/mpi4py_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/mpi4py_install_step.cmake @ONLY)

set(mpi4py_BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/mpi4py_make_step.cmake)
set(mpi4py_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/mpi4py_install_step.cmake)

set(Mpi4py_source "${CMAKE_CURRENT_BINARY_DIR}/build/Mpi4py")

# create an external project to download numpy,
# and configure and build it
ExternalProject_Add(Mpi4py
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${Mpi4py_source}
  URL ${MPI4PY_URL}/${MPI4PY_GZ}
  URL_MD5 ${MPI4PY_MD5}
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/mpi4py
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${mpi4py_BUILD_COMMAND}
  UPDATE_COMMAND ""
  INSTALL_COMMAND ${mpi4py_INSTALL_COMMAND}
  DEPENDS 
    ${Mpi4py_deps}
  ${ep_log_options}
  )

# Mpi4py
#

#ExternalProject_Add(Mpi4py
#  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
#  SOURCE_DIR ${Mpi4py_source}
#  URL ${MPI4PY_URL}/${MPI4PY_GZ}
#  URL_MD5 ${MPI4PY_MD5}
#  BUILD_IN_SOURCE 1
#  CONFIGURE_COMMAND ""
#  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
#  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
#  DEPENDS ${Mpi4py_deps}
#  ${ep_log_options}
#  )
