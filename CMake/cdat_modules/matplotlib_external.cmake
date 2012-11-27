
# Matplotlib
#
set(matplotlib_source_dir "${CMAKE_CURRENT_BINARY_DIR}/build/Matplotlib")

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/matplotlib_patch_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/matplotlib_patch_step.cmake"
  @ONLY
)

set(matplotlib_patch_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/matplotlib_patch_step.cmake)

# Configuration specific
set(INSTALL_DIR "${CMAKE_INSTALL_PREFIX}")

ExternalProject_Add(Matplotlib
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${Matplotlib_SOURCE_DIR}
  URL ${MATPLOTLIB_URL}/${MATPLOTLIB_GZ}
  URL_MD5 ${MATPLOTLIB_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  PATCH_COMMAND ${matplotlib_patch_command}
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${Matplotlib_deps}
  ${ep_log_options}
)
