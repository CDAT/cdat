# create an external project to install pynetcdf
# and configure and build it

# pynetcdf4
#
set(pynetcdf4_source "${CMAKE_CURRENT_BINARY_DIR}/build/pynetcdf4")

ExternalProject_Add(pynetcdf4
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${pynetcdf4_source}
  URL ${PYNETCDF4_URL}/${PYNETCDF4_GZ}
  URL_MD5 ${PYNETCDF4_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND env "NETCDF4_DIR=${cdat_EXTERNALS}" "PYTHONPATH=$ENV{PYTHONPATH}" ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND env "NETCDF4_DIR=${cdat_EXTERNALS}" "PYTHONPATH=$ENV{PYTHONPATH}" "${PYTHON_EXECUTABLE}" setup.py install "${PYTHON_EXTRA_PREFIX}"
  DEPENDS ${pynetcdf4_deps}
  ${ep_log_options}
)
