
# cdms2 tests

add_test("test_departures_time_bounds"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdutil/test_dep_bounds.py
    TestCDATLite)

if(CDAT_DOWNLOAD_SAMPLE_DATA)

endif()

# Tests which should fail
# set_tests_properties(testSo2TasACCESSFails PROPERTIES PASS_REGULAR_EXPRESSION "FAILED")


