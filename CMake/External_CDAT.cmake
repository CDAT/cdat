
set(CDAT_source "${cdat_SOURCE_DIR}")

set(RUNTIME_FLAGS "${cdat_BINARY_DIR}/Externals/lib:$ENV{${LIBRARY_PATH}}")
set(LDFLAGS "-L${cdat_BINARY_DIR}/Externals/NetCDF/lib -L${cdat_BINARY_DIR}/Externals/lib")

get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)

#env EXTERNALS=/Users/partyd/Kitware/uv-cdat/make-file-install//Externals  LDFLAGS="${LDFLAGS/"/} -undefined dynamic_lookup"  /Users/partyd/Kitware/uv-cdat/make-file-install//bin/python install.py  --enable-qt-framework  --with-qt=/Users/partyd/Kitware/uv-cdat/make-file-install//Externals

ExternalProject_Add(CDAT
  DOWNLOAD_DIR ""
  SOURCE_DIR ${cdat_SOURCE_DIR}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND env EXTERNALS=${cdat_BINARY_DIR}/Externals LDFLAGS="${LDFLAGS}" ${LIBRARY_PATH}="${RUNTIME_FLAGS}" ${PYTHON_EXECUTABLE} install.py --enable-qt-framework --with-qt=${QT_ROOT}
  DEPENDS ${CDAT_DEPENDENCIES}
)

