add_test(flake8_dv3d
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/DV3D/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
  --max-line-length=120 # Reasonable line length
)

set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/dv3d")

cdat_add_test(dv3d_vector_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  dv3d_vector_test
  False
  ${BASELINE_DIR}
)


cdat_add_test(dv3d_slider_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  dv3d_slider_test
  False
  ${BASELINE_DIR}
)


#Mesa driver 7.* as issues with these
if (NOT CDAT_BUILD_OFFSCREEN)
    cdat_add_test(Hovmoller_volume_test
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
      Hovmoller_volume_test
      False
      ${BASELINE_DIR}
    )

    cdat_add_test(dv3d_volume_test
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
      dv3d_volume_test
      False
      ${BASELINE_DIR}
    )
    cdat_add_test(dv3d_constituents_test
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
      dv3d_constituents_test
      False
      ${BASELINE_DIR}
    )
endif()


cdat_add_test(dv3d_surface_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  dv3d_surface_test
  False
  ${BASELINE_DIR}
)

cdat_add_test(dv3d_hovmoller_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  dv3d_hovmoller_test
  False
  ${BASELINE_DIR}
)
