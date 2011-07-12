
# PyTables
#
set(PyTables_source "${CMAKE_CURRENT_BINARY_DIR}/build/PyTables")

ExternalProject_Add(PyTables
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${PyTables_source}
  URL ${PYTABLES_URL}/${PYTABLES_GZ}
  URL_MD5 ${PYTABLES_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND  env ${LIBRARY_PATH}=${cdat_EXTERNALS}/lib ${PYTHON_EXECUTABLE} setup.py --hdf5=${cdat_EXTERNALS} build
  INSTALL_COMMAND  env ${LIBRARY_PATH}=${cdat_EXTERNALS}/lib ${PYTHON_EXECUTABLE} setup.py --hdf5=${cdat_EXTERNALS} install
  DEPENDS ${PyTables_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
  )
