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

add_test(vcs_test_clear_empy_canvas
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_clear_empty_canvas.py
  )

add_test(vcs_test_colorcells
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/vcs/test_colorcells.py
  )

message(${CDAT_DOWNLOAD_SAMPLE_DATA} and ${CDAT_BUILD_MATPLOTLIB})

#These test actually plot things need sample data
  ## We need matplotlib to compare images
if (CDAT_BUILD_MATPLOTLIB)
  add_test(vcs_test_1d_missing
    ${CMAKE_INSTALL_PREFIX}/bin/python
    ${cdat_SOURCE_DIR}/testing/vcs/test_1D_missing.py
    ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_1d_missing.png
    )
  add_test(vcs_test_infinity
    ${CMAKE_INSTALL_PREFIX}/bin/python
    ${cdat_SOURCE_DIR}/testing/vcs/test_infinity.py
    ${cdat_SOURCE_DIR}/testing/vcs/infinity.png
    )

#  if (CDAT_DOWNLOAD_SAMPLE_DATA)
    FOREACH(gm boxfill isofill isoline vector meshfill yxvsx xvsy xyvsy oned scatter)
      add_test(vcs_test_basic_${gm}
        ${CMAKE_INSTALL_PREFIX}/bin/python
        ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
        --gm_type=${gm}
        --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_${gm}.png
        )
      add_test(vcs_test_basic_${gm}_masked
        ${CMAKE_INSTALL_PREFIX}/bin/python
        ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
        --gm_type=${gm}
        --mask
        --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_${gm}_masked.png
        )
    ENDFOREACH(gm)
    add_test(vcs_test_basic_isofill_masked_pol_proj
      ${CMAKE_INSTALL_PREFIX}/bin/python
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=isofill
      --mask
      --projection=-3
      --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_isofill_masked_-3_proj.png
      )
    add_test(vcs_test_basic_isofill_masked_aeqd_proj
      ${CMAKE_INSTALL_PREFIX}/bin/python
      ${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_gms.py
      --gm_type=isofill
      --mask
      --projection=aeqd
      --source=${cdat_SOURCE_DIR}/testing/vcs/test_vcs_basic_isofill_masked_aeqd_proj.png
      )
  #endif()
endif()

