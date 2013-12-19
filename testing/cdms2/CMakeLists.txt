
# cdms2 tests

add_test("test_dim_unlimited"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/test_dim_unlimited.py
    TestCDATLite)
foreach(i range 1 6 )
    add_test("CDMS_Test_${i}"
        ${CMAKE_INSTALL_PREFIX}/bin/python 
        ${cdat_SOURCE_DIR}/testing/cdms2/cdtest0${i}.py)
endforeach()

# if(CDAT_DOWNLOAD_SAMPLE_DATA)
# endif()

# Tests which should fail
# set_tests_properties(testSo2TasACCESSFails PROPERTIES PASS_REGULAR_EXPRESSION "FAILED")


