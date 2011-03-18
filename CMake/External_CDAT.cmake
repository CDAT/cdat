
set(CDAT_source "${cdat_SOURCE_DIR}")

set(LDFLAGS "-L${cdat_BINARY_DIR}/Externals/NetCDF/lib -L${cdat_BINARY_DIR}/Externals/lib -L${cdat_BINARY_DIR}/lib")

get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)

#configure_file(${cdat_CMAKE_SOURCE_DIR}/pyopengl_make_step.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/pyopengl_make_step.cmake
#  @ONLY)

#configure_file(${cdat_CMAKE_SOURCE_DIR}/pyopengl_install_step.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/pyopengl_install_step.cmake
#  @ONLY)

#set(PyOpenGL_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pyopengl_make_step.cmake)
#set(PyOpenGL_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/pyopengl_install_step.cmake)

#env EXTERNALS=/Users/partyd/Kitware/uv-cdat/make-file-install/Externals  LDFLAGS="${LDFLAGS/"/} -undefined dynamic_lookup"  /Users/partyd/Kitware/uv-cdat/make-file-install/bin/python install.py  --enable-qt-framework  --with-qt=/Users/partyd/Kitware/uv-cdat/make-file-install/Externals

ExternalProject_Add(CDAT
  DOWNLOAD_DIR ""
  SOURCE_DIR ${CDAT_source}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND env LDFLAGS=${LDFLAGS} ${PYTHON_EXECUTABLE} install.py --enable-qt-framework --with-qt=${QT_ROOT}
  DEPENDS ${CDAT_DEPENDENCIES}
)

set(CDAT_DIR "${PyOpenGL_binary}" CACHE PATH "CDAT binary directory" FORCE)
mark_as_advanced(CDAT_DIR)
