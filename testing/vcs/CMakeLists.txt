#This test verifiy we can plot hurricane markers
add_test(vcs_verify_import
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_import.py
)
add_test(vcs_verify_init
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_init.py
)
