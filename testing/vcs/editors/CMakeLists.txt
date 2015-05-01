set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcs/editors")
set(TEST_DIR "${cdat_SOURCE_DIR}/testing/vcs/editors")

cdat_add_test(vcs_test_editor_marker_delete
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_editor_marker_delete.py
)