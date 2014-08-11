#This test verifiy we can plot hurricane markers
add_test(vcs_verify_import
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_import.py
)
add_test(vcs_verify_init
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_init.py
)
add_test(vcs_verify_boxfill_basics
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_boxfill_basics_no_plot.py
)
add_test(vcs_verify_proj_basics
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_projection_basics_no_plot.py
)
add_test(vcs_verify_fillarea_basics
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_fillarea_basics_no_plot.py
)

message(${CDAT_DOWNLOAD_SAMPLE_DATA} and ${CDAT_BUILD_MATPLOTLIB})

#These test actually plot things need sample data
#if (CDAT_DOWNLOAD_SAMPLE_DATA)
  ## We need matplotlib to compare images
  if (CDAT_BUILD_MATPLOTLIB)
    add_test(vcs_test_basic_boxfill
      ${CMAKE_INSTALL_PREFIX}/bin/python
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=boxfill
      --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_boxfill.png
      )
    add_test(vcs_test_basic_isofill
      ${CMAKE_INSTALL_PREFIX}/bin/python
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=isofill
      --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_isofill.png
      )
    add_test(vcs_test_basic_isoline
      ${CMAKE_INSTALL_PREFIX}/bin/python
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=isoline
      --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_isoline.png
      )
    add_test(vcs_test_basic_meshfill
      ${CMAKE_INSTALL_PREFIX}/bin/python
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=meshfill
      --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_meshfill.png
      )
    add_test(vcs_test_basic_vector
      ${CMAKE_INSTALL_PREFIX}/bin/python
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=vector
      --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_vector.png
      )
  endif()
  #endif()

