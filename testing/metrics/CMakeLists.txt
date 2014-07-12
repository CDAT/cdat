# test from VCS, example of a graphics test
#add_test(vcs_verify_hurricane_marker
#  ${CMAKE_INSTALL_PREFIX}/bin/python
#  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_hurricane_marker.py
#  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_hurricane_marker.png
#)
# test from CDMS2, example of a test script
#add_test("CDMS_Test_01"
#    ${CMAKE_INSTALL_PREFIX}/bin/python 
#    ${cdat_SOURCE_DIR}/testing/cdms2/cdtest01.py)
add_test("diags_test_01"
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/metrics/diagtest01.py
  ${UVCMETRICS_TEST_DATA_DIRECTORY} )
