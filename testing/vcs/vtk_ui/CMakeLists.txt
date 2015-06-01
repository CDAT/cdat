set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcs/vtk_ui")
set(TEST_DIR "${cdat_SOURCE_DIR}/testing/vcs/vtk_ui")

# WIDGET TESTS

# Button tests

cdat_add_test(test_vtk_ui_button_draw
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_draw.py
  ${BASELINE_DIR}/test_vtk_ui_button_draw.png
)

cdat_add_test(test_vtk_ui_button_action
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_action.py
)

cdat_add_test(test_vtk_ui_button_corner_radius
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_corner_radius.py
  ${BASELINE_DIR}/test_vtk_ui_button_corner_radius.png
)

cdat_add_test(test_vtk_ui_button_dimensions
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_dimensions.py
  ${BASELINE_DIR}/test_vtk_ui_button_dimensions.png
)

cdat_add_test(test_vtk_ui_button_states
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_states.py
  ${BASELINE_DIR}/test_vtk_ui_button_states.png
)

cdat_add_test(test_vtk_ui_button_click
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_click.py
)

cdat_add_test(test_vtk_ui_button_font
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_font.py
  ${BASELINE_DIR}/test_vtk_ui_button_font.png
)

cdat_add_test(test_vtk_ui_button_alignment
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_alignment.py
  ${BASELINE_DIR}/test_vtk_ui_button_alignment.png
)

cdat_add_test(test_vtk_ui_button_image
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_image.py
  ${BASELINE_DIR}/test_vtk_ui_button_image.png
)

cdat_add_test(test_vtk_ui_button_tooltip_show
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_tooltip_show.py
  ${BASELINE_DIR}/test_vtk_ui_button_tooltip_show.png
)

cdat_add_test(test_vtk_ui_button_tooltip_hide
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_tooltip_hide.py
  ${BASELINE_DIR}/test_vtk_ui_button_tooltip_hide.png
)

cdat_add_test(test_vtk_ui_button_state_advance
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_button_state_advance.py
)

cdat_add_test(test_vtk_ui_toggle_button_get_text
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_toggle_button_get_text.py
)

# Handle tests

cdat_add_test(test_vtk_ui_handle_appearance
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_handle_appearance.py
  ${BASELINE_DIR}/test_vtk_ui_handle_appearance.png
)

cdat_add_test(test_vtk_ui_handle_normalize
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_handle_normalize.py
)

cdat_add_test(test_vtk_ui_handle_sizing
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_handle_sizing.py
  ${BASELINE_DIR}/test_vtk_ui_handle_sizing.png
)

cdat_add_test(test_vtk_ui_handle_interaction
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_handle_interaction.py
)

# Textbox Tests

cdat_add_test(test_vtk_ui_textbox_highlight
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_textbox_highlight.py
  ${BASELINE_DIR}/test_vtk_ui_textbox_highlight.png
)

cdat_add_test(test_vtk_ui_textbox_appearance
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_textbox_appearance.py
  ${BASELINE_DIR}/test_vtk_ui_textbox_appearance.png
)

cdat_add_test(test_vtk_ui_textbox_blank_text
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_textbox_blank_text.py
)

cdat_add_test(test_vtk_ui_textbox_cursor
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_textbox_cursor.py
  ${BASELINE_DIR}/test_vtk_ui_textbox_cursor.png
)

cdat_add_test(test_vtk_ui_textbox_typing
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_textbox_typing.py
)

cdat_add_test(test_vtk_ui_textbox_coordinate_editing
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_textbox_coordinate_editing.py
)

cdat_add_test(test_vtk_ui_textbox_detach
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_textbox_detach.py
)

# Label tests

cdat_add_test(test_vtk_ui_label_top
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_top.py
  ${BASELINE_DIR}/test_vtk_ui_label_top.png
)

cdat_add_test(test_vtk_ui_label_y
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_y.py
  ${BASELINE_DIR}/test_vtk_ui_label_y.png
)

cdat_add_test(test_vtk_ui_label_behaviors
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_behaviors.py
  ${BASELINE_DIR}/test_vtk_ui_label_behaviors.png
)

cdat_add_test(test_vtk_ui_label_bounds
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_bounds.py
)

cdat_add_test(test_vtk_ui_label_detach
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_detach.py
  ${BASELINE_DIR}/test_vtk_ui_label_detach.png
)

cdat_add_test(test_vtk_ui_label_font_attrs
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_font_attrs.py
  ${BASELINE_DIR}/test_vtk_ui_label_font_attrs.png
)

cdat_add_test(test_vtk_ui_label_left
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_left.py
  ${BASELINE_DIR}/test_vtk_ui_label_left.png
)

cdat_add_test(test_vtk_ui_label_render
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_render.py
  ${BASELINE_DIR}/test_vtk_ui_label_render_initial.png
  ${BASELINE_DIR}/test_vtk_ui_label_render_color_change.png
)

cdat_add_test(test_vtk_ui_label_show_hide
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_show_hide.py
  ${BASELINE_DIR}/test_vtk_ui_label_show_hide.png
)

cdat_add_test(test_vtk_ui_label_text
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_text.py
  ${BASELINE_DIR}/test_vtk_ui_label_text.png
)

cdat_add_test(test_vtk_ui_label_x
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_label_x.py
  ${BASELINE_DIR}/test_vtk_ui_label_x.png
)

# Text Utility Tests

cdat_add_test(test_vtk_ui_contrasting_colors
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_contrasting_colors.py
)

cdat_add_test(test_vtk_ui_hsv_rgb
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_hsv_rgb.py
)

cdat_add_test(test_vtk_ui_set_font
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_set_font.py
)

cdat_add_test(test_vtk_ui_text_dimensions
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_text_dimensions.py
)

cdat_add_test(test_vtk_ui_white_or_black
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_white_or_black.py
)

# Toolbar Tests

cdat_add_test(test_vtk_ui_toolbar_in_toolbar_open
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_toolbar_in_toolbar_open.py
  ${BASELINE_DIR}/test_vtk_ui_toolbar_in_toolbar_open.png
)

cdat_add_test(test_vtk_ui_toolbar_in_toolbar_closed
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_toolbar_in_toolbar_closed.py
  ${BASELINE_DIR}/test_vtk_ui_toolbar_in_toolbar_closed.png
)

cdat_add_test(test_vtk_ui_toolbar_label
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_toolbar_label.py
  ${BASELINE_DIR}/test_vtk_ui_toolbar_label.png
)

cdat_add_test(test_vtk_ui_toolbar_open
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_toolbar_open.py
  ${BASELINE_DIR}/test_vtk_ui_toolbar_open.png
)

cdat_add_test(test_vtk_ui_toolbar_close
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_toolbar_close.py
  ${BASELINE_DIR}/test_vtk_ui_toolbar_close.png
)

# Slider tests

cdat_add_test(test_vtk_ui_slider_value_func
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_slider_value_func.py
  ${BASELINE_DIR}/test_vtk_ui_slider_value_func.png
)

cdat_add_test(test_vtk_ui_slider_values
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_slider_values.py
  ${BASELINE_DIR}/test_vtk_ui_slider_values.png
)

cdat_add_test(test_vtk_ui_slider_appearance
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_slider_appearance.py
  ${BASELINE_DIR}/test_vtk_ui_slider_appearance.png
)

cdat_add_test(test_vtk_ui_slider_callbacks
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_slider_callbacks.py
)

cdat_add_test(test_vtk_ui_slider_place
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_slider_place.py
  ${BASELINE_DIR}/test_vtk_ui_slider_place.png
)

cdat_add_test(test_vtk_ui_slider_jumps
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_slider_jumps.py
  ${BASELINE_DIR}/test_vtk_ui_slider_jumps.png
)

# Manager Tests

cdat_add_test(test_vtk_ui_manager_resize
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_manager_resize.py
  ${BASELINE_DIR}/test_vtk_ui_manager_resize.png
)

# Base class tests

cdat_add_test(test_vtk_ui_behavior_keyable
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_behavior_keyable.py
)

cdat_add_test(test_vtk_ui_widget_init
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_widget_init.py
)

cdat_add_test(test_vtk_ui_widget_show_hide
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_widget_show_hide.py
  ${BASELINE_DIR}/test_vtk_ui_widget_show.png
  ${BASELINE_DIR}/test_vtk_ui_widget_hide.png
)

cdat_add_test(test_vtk_ui_widget_subscribe_unsubscribe
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_widget_subscribe_unsubscribe.py
)

cdat_add_test(test_vtk_ui_widget_detach
  "${PYTHON_EXECUTABLE}"
  ${TEST_DIR}/test_vtk_ui_widget_detach.py
)
