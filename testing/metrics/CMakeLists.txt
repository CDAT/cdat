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

  cdat_add_test("diags_test_03"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest03.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_04"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest04.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_41"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest41.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_05"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest05.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_06"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest06.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_07"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest07.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_08"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest08.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_09"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest09.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_10"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest10.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_11"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest11.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )
    
  cdat_add_test("diags_test_12"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest12.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_13"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest13.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )

  cdat_add_test("diags_test_15"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/metrics/diagtest15.py
    --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY}/
    --baseline=${BASELINE_DIR}/metrics/ )
else()
  message("[INFO] You did not download UVCMETRICS Test data not test will be run")
endif()
