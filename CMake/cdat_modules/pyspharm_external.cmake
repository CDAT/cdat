
# Pyspharm
#
set(pyspharm_source "${CMAKE_CURRENT_BINARY_DIR}/build/pyspharm")
ExternalProject_Add(pyspharm
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${pyspharm_source}
  URL ${PYSPHARM_URL}/${PYSPHARM_GZ}
  URL_MD5 ${PYSPHARM_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} ${PYTHON_EXECUTABLE} setup.py build 
  INSTALL_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} ${PYTHON_EXECUTABLE} setup.py install 
  DEPENDS ${pyspharm_deps}
  ${ep_log_options}
  )
