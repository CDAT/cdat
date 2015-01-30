set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/dv3d")

add_test(dv3d_slider_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  'dv3d_slider_test'
  '${BASELINE_DIR}'
)


add_test(dv3d_volume_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  'dv3d_volume_test'
  '${BASELINE_DIR}'
)


add_test(dv3d_surface_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  'dv3d_surface_test'
  '${BASELINE_DIR}'
)


add_test(dv3d_constituents_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  'dv3d_constituents_test'
  '${BASELINE_DIR}'
)


add_test(dv3d_hovmoller_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  'dv3d_hovmoller_test'
  '${BASELINE_DIR}'
)


add_test(dv3d_hovmoller_test_1
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py
  'dv3d_hovmoller_test_1'
  '${BASELINE_DIR}'
)


