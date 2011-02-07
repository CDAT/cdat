
set(PyOpenGL_source "${CMAKE_CURRENT_BINARY_DIR}/PyOpenGL")
set(PyOpenGL_binary "${CMAKE_CURRENT_BINARY_DIR}/PyOpenGL-build")
set(PyOpenGL_install "${CMAKE_CURRENT_BINARY_DIR}/PyOpenGL-install")

ExternalProject_Add(PyOpenGL
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${PyOpenGL_source}
  BINARY_DIR ${PyOpenGL_build}
  INSTALL_DIR ${PyOpenGL_install}
  URL ${PYOPENGL_URL}/${PYOPENGL_GZ}
  URL_MD5 ${PYOPENGL_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(PyOpenGL_DIR "${PyOpenGL_binary}" CACHE PATH "PyOpenGL binary directory" FORCE)
mark_as_advanced(PyOpenGL_DIR)
