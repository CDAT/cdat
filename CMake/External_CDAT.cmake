
set(CDAT_source "${cdat_SOURCE_DIR}")

set(RUNTIME_FLAGS "${cdat_BINARY_DIR}/Externals/lib:$ENV{${LIBRARY_PATH}}")
set(LDFLAGS "-L${cdat_BINARY_DIR}/Externals/NetCDF/lib -L${cdat_BINARY_DIR}/Externals/lib -L${cdat_BINARY_DIR}/lib")

get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)

ExternalProject_Add(CDAT
  DOWNLOAD_DIR ""
  SOURCE_DIR ${cdat_SOURCE_DIR}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND env LDFLAGS=${LDFLAGS} ${LIBRARY_PATH}=${RUNTIME_FLAGS} ${PYTHON_EXECUTABLE} install.py --enable-qt-framework --with-qt=${QT_ROOT}
  DEPENDS ${CDAT_DEPENDENCIES}
)

