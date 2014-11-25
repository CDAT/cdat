add_test(dv3d_slider_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_slider_test.py
)


add_test(dv3d_volume_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_volume_test.py
)


add_test(dv3d_surface_test
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_surface_test.py
)


add_test(dv3d_constituents_test
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_constituents_test.py
)


add_test(dv3d_hovmoller_test
  ${CMAKE_INSTALL_PREFIX}/bin/python
  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_hovmoller_test.py
)


