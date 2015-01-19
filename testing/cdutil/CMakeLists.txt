
# cdutil tests

add_test("test_departures_time_bounds"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_dep_bounds.py
    TestCDATLite)

if(CDAT_DOWNLOAD_SAMPLE_DATA)
  add_test("test_VariablesMatcher_1"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_VariablesMatcher_1.py)
  add_test("test_VariablesMatcher_2"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_VariablesMatcher_2.py)
  add_test("test_VariablesMatcher_3"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_VariablesMatcher_3.py)
  add_test("test_averager"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_averager.py)
  add_test("test_daily_bounds"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_daily_bounds.py)
  add_test("test_gen_mask"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_gen_mask.py)
  add_test("test_keep_missing_value"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_keep_missing_value.py)
  add_test("test_missing_season"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_missing_season.py)
  add_test("test_sftbyrgn"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_sftbyrgn.py)
  add_test("test_times"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_times.py)
  add_test("test_times_2"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_times_2.py)
  add_test("test_times_3"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_times_3.py)
  add_test("test_vert"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_vert.py)
  add_test("test_years"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/cdutil/test_years.py)
endif()

# Tests which should fail
# set_tests_properties(testSo2TasACCESSFails PROPERTIES PASS_REGULAR_EXPRESSION "FAILED")


