# test from VCS, example of a graphics test
#cdat_add_test(vcs_verify_hurricane_marker
#  "${PYTHON_EXECUTABLE}"
#  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_hurricane_marker.py
#  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_hurricane_marker.png
#)
# test from CDMS2, example of a test script
#cdat_add_test("CDMS_Test_01"
#    "${PYTHON_EXECUTABLE}"
#    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest01.py)
cdat_add_test("diags_test_01"
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/metrics/diagtest01.py
  ${UVCMETRICS_TEST_DATA_DIRECTORY} )
cdat_add_test("diags_test_02"
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/metrics/diagtest01.py
  ${UVCMETRICS_TEST_DATA_DIRECTORY} )
