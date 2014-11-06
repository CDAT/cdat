set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcsaddons")

add_test(vcs_addons_preview_2x2
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcsaddons/test_vcsaddons_preview_2x2.py
  ${BASELINE_DIR}/test_vcsaddons_preview_2x2.png
)
add_test(vcs_addons_test_12_plot_one_leg_per_row
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcsaddons/test_12_plot_one_leg_per_row.py
  ${BASELINE_DIR}/test_12_plot_one_leg_per_row.png
)
add_test(vcs_addons_test_12_plot_one_leg_per_row_right
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcsaddons/test_12_plot_one_leg_per_row_right.py
  ${BASELINE_DIR}/test_12_plot_one_leg_per_row_right.png
)
add_test(vcs_addons_test_EzTemplate_12_plots_margins_thickness
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcsaddons/test_EzTemplate_12_plots_margins_thickness.py
  ${BASELINE_DIR}/test_EzTemplate_12_plots_margins_thickness.png
)
add_test(vcs_addons_test_EzTemplate_12_plots_legd_direction
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcsaddons/test_EzTemplate_12_plots_legd_direction.py
  ${BASELINE_DIR}/test_EzTemplate_12_plots_legd_direction.png
)
add_test(vcs_addons_test_EzTemplate_12_plots_mix_glb_local
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcsaddons/test_EzTemplate_12_plots_mix_glb_local.py
  ${BASELINE_DIR}/test_EzTemplate_12_plots_mix_glb_local.png
)
add_test(vcs_addons_test_EzTemplate_12_plots_spacing
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcsaddons/test_EzTemplate_12_plots_spacing.py
  ${BASELINE_DIR}/test_EzTemplate_12_plots_spacing.png
)

if (CDAT_DOWNLOAD_SAMPLE_DATA)
  add_test(vcs_addons_EzTemplate_2x2
    ${CMAKE_INSTALL_PREFIX}/bin/python
    ${cdat_SOURCE_DIR}/testing/vcsaddons/test_vcs_addons_EzTemplate_2x2.py
    ${BASELINE_DIR}/test_vcs_addons_EzTemplate_2x2.png
  )
endif()
