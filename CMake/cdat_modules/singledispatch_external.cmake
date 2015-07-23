# The singledispatch project

set(singledispatch_binary "${CMAKE_CURRENT_BINARY_DIR}/build/singledispatch")

ExternalProject_Add(singledispatch
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${singledispatch_binary}
  URL ${SINGLEDISPATCH_SOURCE}
  URL_MD5 ${SINGLEDISPATCH_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  DEPENDS ${singledispatch_deps}
  ${ep_log_options}
  )
