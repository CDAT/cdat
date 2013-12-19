#This test verifiy we can plot hurricane markers
add_test(vcs_verify_hurricane_marker
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_hurricane_marker.py
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_hurricane_marker.png
)
add_test(vcs_test_anim_basic
    ${CMAKE_INSTALL_PREFIX}/bin/python
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_anim_basic.py
    ${cdat_SOURCE_DIR}/testing/vcs/test_anim_basic_out_%i.png
    )
