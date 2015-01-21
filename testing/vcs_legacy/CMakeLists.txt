#This test verifiy we can plot hurricane markers
add_test(vcs_verify_hurricane_marker
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_hurricane_marker.py
  "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcs_legacy/test_vcs_hurricane_marker.png"
)
