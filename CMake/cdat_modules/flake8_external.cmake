ExternalProject_Add(flake8
  DOWNLOAD_DIR "${CMAKE_CURRENT_BINARY_DIR}"
  SOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/build/flake8"
  URL "${FLAKE8_SOURCE}"
  URL_MD5 ${FLAKE8_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND "${PYTHON_EXECUTABLE}" setup.py build
  INSTALL_COMMAND "${PYTHON_EXECUTABLE}" setup.py install "${PYTHON_EXTRA_PREFIX}"
  DEPENDS ${flake8_deps}
  ${ep_log_options}
  )

if (APPLE)
  set(FLAKE8_EXECUTABLE
    "${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/bin/flake8")
else()
  set(FLAKE8_EXECUTABLE "${CMAKE_INSTALL_PREFIX}/bin/flake8")
endif()
