set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcs/editors")
set(TEST_DIR "${cdat_SOURCE_DIR}/testing/vcs/editors")

cdat_add_test(vcs_test_editor_marker_delete
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vcs_editor_marker_delete.py
)

cdat_add_test(vcs_test_editor_font
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vcs_editor_font.py
)
