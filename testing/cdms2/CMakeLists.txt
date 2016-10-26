
# cdms2 tests
FILE(GLOB cdms_tests  ${cdat_BINARY_DIR}/cdms/Test/*.py)
foreach(cdms_test ${cdms_tests})
    cdat_add_test("${cdms_test}" "${PYTHON_EXECUTABLE}" ${cdat_BINARY_DIR}/cdms/run_tests.py ${cdms_test})
endforeach()


