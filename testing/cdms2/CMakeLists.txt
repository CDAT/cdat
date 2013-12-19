
# cdms2 tests

add_test("test_dim_unlimited"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/test_dim_unlimited.py
    TestCDATLite)
add_test("CDMS_Test_01"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest01.py)

add_test("CDMS_Test_02"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest02.py)

add_test("CDMS_Test_03"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest03.py)

add_test("CDMS_Test_04"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest04.py)

add_test("CDMS_Test_05"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest05.py)

add_test("CDMS_Test_06"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest06.py)

add_test("CDMS_Test_07"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest07.py)

add_test("CDMS_Test_08"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest08.py)

add_test("CDMS_Test_09"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest09.py)

add_test("CDMS_Test_10"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest10.py)

add_test("CDMS_Test_11"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest11.py)

add_test("CDMS_Test_12"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest12.py)

add_test("CDMS_Test_13"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest13.py)

add_test("CDMS_Test_14"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest14.py)

add_test("CDMS_Test_15"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest15.py)

add_test("CDMS_Test_16"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest16.py)

add_test("CDMS_Test_17"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest17.py)

add_test("CDMS_Test_18"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest18.py)

if(CDAT_DOWNLOAD_SAMPLE_DATA)
    add_test("CDMS_Test_JSON"
        ${CMAKE_INSTALL_PREFIX}/bin/python 
        ${cdat_SOURCE_DIR}/testing/cdms2/test_dump_load_jsn.py)

endif()

add_test("CDMS_Test_multiple_formats"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/test_all_formats.py)

add_test("CDMS_Test_write_compressed"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/test_compression.py)

add_test("CDMS_Test_Aurore"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/test_aurore.py)

add_test("CDMS_Test_setting_bad_cal"
    ${CMAKE_INSTALL_PREFIX}/bin/python 
    ${cdat_SOURCE_DIR}/testing/cdms2/test_cal_bug.py)

# Tests which should fail
# set_tests_properties(testSo2TasACCESSFails PROPERTIES PASS_REGULAR_EXPRESSION "FAILED")


