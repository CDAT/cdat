set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/dv3d")

add_test(dv3d_slider_test
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_slider_test.py
  "--baselinedir=${BASELINE_DIR}"
)

add_test(dv3d_volume_test
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_volume_test.py
  "--baselinedir=${BASELINE_DIR}"
)

add_test(dv3d_surface_test
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_surface_test.py
  "--baselinedir=${BASELINE_DIR}"
)


