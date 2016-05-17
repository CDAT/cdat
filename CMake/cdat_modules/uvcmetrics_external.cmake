set(uvcmetrics_source "${CMAKE_CURRENT_BINARY_DIR}/build/uvcmetrics")
set(uvcmetrics_binary "${CMAKE_CURRENT_BINARY_DIR}/build/uvcmetrics")
set(uvcmetrics_install "${cdat_EXTERNALS}")

set(GIT_CMD_STR GIT_REPOSITORY "${UVCMETRICS_SOURCE}")

ExternalProject_Add(UVCMETRICS
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${uvcmetrics_source}
  INSTALL_DIR ${uvcmetrics_install}
  ${GIT_CMD_STR}
  GIT_TAG ${UVCMETRICS_BRANCH}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DUVCMETRICS_TEST_DATA_DIRECTORY=${UVCMETRICS_TEST_DATA_DIRECTORY}
  BUILD_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} ${PYTHON_EXECUTABLE} setup.py build 
  INSTALL_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} ${PYTHON_EXECUTABLE} setup.py install 
  DEPENDS ${UVCMETRICS_deps}
  ${ep_log_options}
  )

#add_subdirectory(${uvcmetrics_source}/test)
#ExternalProject_Add_Step(UVCMETRICS ctest
#  COMMAND ${CMAKE_COMMAND} -E create_symlink ${cdat_EXTERNALS}/include/freetype2/freetype ${cdat_EXTERNALS}/include/freetype
#  COMMENT "Symlink include/freetype2/freetype include directory as include/freetype"
#  DEPENDEES install
#)
