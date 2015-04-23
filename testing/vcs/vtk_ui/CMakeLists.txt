set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcs/vtk_ui")
set(TEST_DIR "${cdat_SOURCE_DIR}/testing/vcs/vtk_ui")

# Button tests
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

add_test(test_vtk_ui_button_dimensions
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_dimensions.py
  ${BASELINE_DIR}/test_vtk_ui_button_dimensions.png
)

add_test(test_vtk_ui_button_states
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_states.py
  ${BASELINE_DIR}/test_vtk_ui_button_states.png
)

add_test(test_vtk_ui_button_click
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_click.py
)

add_test(test_vtk_ui_button_font
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_font.py
  ${BASELINE_DIR}/test_vtk_ui_button_font.png
)

add_test(test_vtk_ui_button_alignment
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_alignment.py
  ${BASELINE_DIR}/test_vtk_ui_button_alignment.png
)

add_test(test_vtk_ui_button_image
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_image.py
  ${BASELINE_DIR}/test_vtk_ui_button_image.png
)

add_test(test_vtk_ui_button_tooltip_show
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_tooltip_show.py
  ${BASELINE_DIR}/test_vtk_ui_button_tooltip_show.png
)

add_test(test_vtk_ui_button_tooltip_hide
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_tooltip_hide.py
  ${BASELINE_DIR}/test_vtk_ui_button_tooltip_hide.png
)

add_test(test_vtk_ui_slider_jumps
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_slider_jumps.py
  ${BASELINE_DIR}/test_vtk_ui_slider_jumps.png
)

add_test(test_vtk_ui_textbox_highlight
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_textbox_highlight.py
  ${BASELINE_DIR}/test_vtk_ui_textbox_highlight.png
)

add_test(test_vtk_ui_label_top
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_top.py
  ${BASELINE_DIR}/test_vtk_ui_label_top.png
)
add_test(test_vtk_ui_label_y
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_y.py
  ${BASELINE_DIR}/test_vtk_ui_label_y.png
)

add_test(test_vtk_ui_contrasting_colors
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_complementary_colors.py
)