set(setuptools_source "${CMAKE_CURRENT_BINARY_DIR}/build/setuptools")
set(setuptools_install "${cdat_EXTERNALS}")

# 2012-03-19 C. Doutriaux Commented this out seems to not be able to pick pythonpath and ldlibrarypath
# Seems to be way too complicated for what's  really needed
#configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/setuptools_make_step.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/setuptools_make_step.cmake
#  @ONLY)

#configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/setuptools_install_step.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/setuptools_install_step.cmake
#  @ONLY)

#set(setuptools_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/setuptools_make_step.cmake)
#set(setuptools_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/setuptools_install_step.cmake)
  

# old cmnd
#  BUILD_COMMAND 
#  INSTALL_COMMAND ${setuptools_install_command}

ExternalProject_Add(setuptools
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${setuptools_source}
  INSTALL_DIR ${setuptools_install}
  URL ${SETUPTOOLS_URL}/${SETUPTOOLS_GZ}
  URL_MD5 ${SETUPTOOLS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${setuptools_deps}
  ${ep_log_options}
)

set(setuptools_DIR "${setuptools_binary}" CACHE PATH "setuptools binary directory" FORCE)
mark_as_advanced(setuptools_DIR)
