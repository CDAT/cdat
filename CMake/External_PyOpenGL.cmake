
set(PyOpenGL_source "${CMAKE_CURRENT_BINARY_DIR}/PyOpenGL")
set(PyOpenGL_install "${CMAKE_CURRENT_BINARY_DIR}/PyOpenGL-install")

configure_file(${cdat_CMAKE_SOURCE_DIR}/pyopengl_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/pyopengl_make_step.cmake
  @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/pyopengl_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/pyopengl_install_step.cmake
  @ONLY)

set(Pmw_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pyopengl_make_step.cmake)
set(Pmw_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pyopengl_install_step.cmake)

ExternalProject_Add(PyOpenGL
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${PyOpenGL_source}
  URL ${PYOPENGL_URL}/${PYOPENGL_GZ}
  URL_MD5 ${PYOPENGL_MD5}
  BUILD_IN_SOURCE 1
  BUILD_COMMAND ${Pmw_build_command}
  INSTALL_COMMAND ${Pmw_install_command}
)

set(PyOpenGL_DIR "${PyOpenGL_binary}" CACHE PATH "PyOpenGL binary directory" FORCE)
mark_as_advanced(PyOpenGL_DIR)
