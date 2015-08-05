set(ocgis_source "${CMAKE_CURRENT_BINARY_DIR}/build/ocgis")
set(ocgis_install "${cdat_EXTERNALS}")

ExternalProject_Add(ocgis
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ocgis_source}
  INSTALL_DIR ${ocgis_install}
  BUILD_IN_SOURCE 1
  ${GIT_CMD_STR_OCGIS}
  ${GIT_TAG}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND env "PYTHONPATH=$ENV{PYTHONPATH}" "${PYTHON_EXECUTABLE}" setup.py install "${PYTHON_EXTRA_PREFIX}"
  DEPENDS ${ocgis_deps}
  ${ep_log_options}
)
if (DEFINED GIT_CMD_STR_OCGIS)
  unset(GIT_CMD_STR_OCGIS)
endif()
