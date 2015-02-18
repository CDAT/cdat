
# cdms2 tests

add_test("test_ascontiguous_regrid_nan_issue"
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/cdms2/test_ascontiguous_regrid_nan_issue.py
  )
add_test("test_dim_unlimited"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/test_dim_unlimited.py
    TestCDATLite)
add_test("CDMS_Test_01"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest01.py)

add_test("CDMS_Test_02"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest02.py)

add_test("CDMS_Test_03"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest03.py)

add_test("CDMS_Test_04"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest04.py)

add_test("CDMS_Test_05"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest05.py)

add_test("CDMS_Test_06"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest06.py)

add_test("CDMS_Test_07"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest07.py)

add_test("CDMS_Test_08"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest08.py)

add_test("CDMS_Test_09"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest09.py)

add_test("CDMS_Test_10"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest10.py)

add_test("CDMS_Test_11"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest11.py)

add_test("CDMS_Test_12"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest12.py)

add_test("CDMS_Test_13"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest13.py)

add_test("CDMS_Test_14"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest14.py)

add_test("CDMS_Test_15"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest15.py)

add_test("CDMS_Test_16"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest16.py)

add_test("CDMS_Test_17"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest17.py)

add_test("CDMS_Test_18"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest18.py)

if(CDAT_DOWNLOAD_SAMPLE_DATA)
    add_test("CDMS_Test_JSON"
        "${PYTHON_EXECUTABLE}"
        ${cdat_SOURCE_DIR}/testing/cdms2/test_dump_load_jsn.py)

endif()

add_test("CDMS_Test_multiple_formats"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/test_all_formats.py)

add_test("CDMS_Test_write_compressed"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/test_compression.py)

add_test("CDMS_Test_Aurore"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/test_aurore.py)

add_test("CDMS_Test_setting_bad_cal"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/test_cal_bug.py)

add_test("CDMS_Test_detecting_axes_type"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/test_isLatLonLevTim.py)

add_test("CDMS_Test_pack"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdms2/test_pack_data.py)

add_test("CDMS_asdatetime"
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/cdms2/test_cdms2_axis_as_datetime.py)

add_test("test_cdms2_full_averagering_full_missing"
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/cdms2/test_full_averaging.py
  )

# Tests which should fail
# set_tests_properties(testSo2TasACCESSFails PROPERTIES PASS_REGULAR_EXPRESSION "FAILED")


