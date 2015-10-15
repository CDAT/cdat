# create an external project to install MyProxyClient,
# and configure and build it
if (CDAT_DOWNLOAD_UVCMETRICS_TESTDATA)
  configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/fetch_uvcmetrics_testdata.py.in
    ${cdat_CMAKE_BINARY_DIR}/fetch_uvcmetrics_testdata.py
    @ONLY)

  execute_process(
    COMMAND ${PYTHON_EXECUTABLE} ${cdat_CMAKE_BINARY_DIR}/fetch_uvcmetrics_testdata.py
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
    RESULT_VARIABLE res
    OUTPUT_VARIABLE ver
    )

  if (NOT ${res} EQUAL 0)
    message("[INFO] Failed to fetch test data for uvcmetrics, tests will fail")
  endif()
endif()


set(GIT_CMD_STR GIT_REPOSITORY "${UVCMETRICS_SOURCE}")
set(GIT_TAG GIT_TAG "${UVCMETRICS_BRANCH}")
set(nm UVCMETRICS)
set(OLD OFF)
include(pipinstaller)
unset(OLD)
