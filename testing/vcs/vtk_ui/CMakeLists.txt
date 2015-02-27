set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcs/vtk_ui")
set(TEST_DIR "${cdat_SOURCE_DIR}/testing/vcs/vtk_ui")

add_test(test_vtk_ui_button_draw
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_draw.py
  ${BASELINE_DIR}/test_vtk_ui_button_draw.png
)

add_test(test_vtk_ui_button_action
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_action.py
)

add_test(test_vtk_ui_button_corner_radius
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_corner_radius.py
  ${BASELINE_DIR}/test_vtk_ui_button_corner_radius.png
)