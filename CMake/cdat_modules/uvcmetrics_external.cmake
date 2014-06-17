# create an external project to install MyProxyClient,
# and configure and build it
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
  message("Couldn't fetch test files for uvcmetrics package, tests will fail")
endif()
set(nm UVCMETRICS)
include(pipinstaller)
