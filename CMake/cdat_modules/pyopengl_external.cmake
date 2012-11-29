
set(PyOpenGL_source "${CMAKE_CURRENT_BINARY_DIR}/PyOpenGL")
set(PyOpenGL_install "${cdat_EXTERNALS}")

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/pyopengl_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/pyopengl_make_step.cmake
  @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/pyopengl_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/pyopengl_install_step.cmake
  @ONLY)

set(PyOpenGL_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pyopengl_make_step.cmake)
set(PyOpenGL_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pyopengl_install_step.cmake)

ExternalProject_Add(PyOpenGL
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${PyOpenGL_source}
  URL ${PYOPENGL_URL}/${PYOPENGL_GZ}
  URL_MD5 ${PYOPENGL_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PyOpenGL_build_command}
  INSTALL_COMMAND ${PyOpenGL_install_command}
  DEPENDS ${PyOpenGL_deps}
  ${ep_log_options}
)

