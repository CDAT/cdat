set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/")

cdat_add_test("diags_test_01"
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/metrics/diagtest01.py
  --datadir=${UVCMETRICS_TEST_DATA_DIRECTORY} 
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
  --baseline=${BASELINE_DIR}/metrics/)

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