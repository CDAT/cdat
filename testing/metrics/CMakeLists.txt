set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/")
if (CDAT_DOWNLOAD_UVCMETRICS_TESTDATA)
  cdat_add_test("diags_test_01"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest01.py
    ${UVCMETRICS_TEST_DATA_DIRECTORY}
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_02"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest02.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_02 PROPERTIES DEPENDS diags_test_01)

  cdat_add_test("diags_test_03"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest03.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_03 PROPERTIES DEPENDS diags_test_02)

  cdat_add_test("diags_test_04"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest04.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_04 PROPERTIES DEPENDS diags_test_03)

  cdat_add_test("diags_test_41"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest41.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_41 PROPERTIES DEPENDS diags_test_04)

  cdat_add_test("diags_test_05"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest05.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_05 PROPERTIES DEPENDS diags_test_41)

  cdat_add_test("diags_test_06"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest06.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_06 PROPERTIES DEPENDS diags_test_05)

  cdat_add_test("diags_test_07"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest07.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_07 PROPERTIES DEPENDS diags_test_06)

  cdat_add_test("diags_test_08"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest08.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_08 PROPERTIES DEPENDS diags_test_07)

  cdat_add_test("diags_test_09"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest09.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_09 PROPERTIES DEPENDS diags_test_08)

  cdat_add_test("diags_test_10"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest10.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_10 PROPERTIES DEPENDS diags_test_09)

  cdat_add_test("diags_test_11"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest11.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )
    
  set_tests_properties(diags_test_11 PROPERTIES DEPENDS diags_test_10)

  cdat_add_test("diags_test_12"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest12.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_12 PROPERTIES DEPENDS diags_test_11)

  cdat_add_test("diags_test_13"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest13.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  set_tests_properties(diags_test_13 PROPERTIES DEPENDS diags_test_12)

  cdat_add_test("diags_test_15"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest15.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )
  set_tests_properties(diags_test_15 PROPERTIES DEPENDS diags_test_13)

else()
  message("[INFO] You did not download UVCMETRICS Test data not test will be run")
endif()
