set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/vcsaddons")

add_test(vcs_addons_preview_2x2
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcsaddons/test_vcsaddons_preview_2x2.py
  "${BASELINE_DIR}/test_vcsaddons_preview_2x2.png"
)

if (CDAT_DOWNLOAD_SAMPLE_DATA)
  add_test(vcs_addons_EzTemplate_2x2
    ${CMAKE_INSTALL_PREFIX}/bin/python
    ${cdat_SOURCE_DIR}/testing/vcsaddons/test_vcs_addons_EzTemplate_2x2.py
    "${BASELINE_DIR}/test_vcs_addons_EzTemplate_2x2.png"
  )
endif()
