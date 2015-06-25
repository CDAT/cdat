# The pep8 project

set(pep8_binary "${CMAKE_CURRENT_BINARY_DIR}/build/pep8")

ExternalProject_Add(pep8
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${pep8_binary}
  URL ${PEP8_SOURCE}
  URL_MD5 ${PEP8_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${pep8_deps}
  ${ep_log_options}
  )
