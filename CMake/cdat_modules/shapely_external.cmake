
# Shapely
#
set(Shapely_source "${CMAKE_CURRENT_BINARY_DIR}/build/Shapely")

ExternalProject_Add(Shapely
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${Shapely_source}
  URL ${SHAPELY_URL}/${SHAPELY_GZ}
  URL_MD5 ${SHAPELY_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${Shapely_deps}
  ${ep_log_options}
  )
