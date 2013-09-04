#this test verifies that uvcdat starts up and exits cleanly
add_test(run_uvcdat ${CMAKE_INSTALL_PREFIX}/bin/uvcdat -P -N -T 2 -S ${CMAKE_BINARY_DIR}/tmp)

#this test verifies that we can open a file and show it
add_test(cdms_test
  ${CMAKE_INSTALL_PREFIX}/bin/uvcdat
  -S ${CMAKE_BINARY_DIR}/tmp
  -b ${cdat_SOURCE_DIR}/testing/uvcdat/cdms_test.vt:test
  -a "datafile=${cdat_SOURCE_DIR}/libcdms/src/cdunif/test/testnc.nc")

#the same as cdms_test but verify result image is correct
add_test(cdms_verify
  ${CMAKE_INSTALL_PREFIX}/bin/runpytest
  ${CMAKE_BINARY_DIR}/tmp
  ${cdat_SOURCE_DIR}/testing/uvcdat/cdms_test.vt:test
  "datafile=${cdat_SOURCE_DIR}/libcdms/src/cdunif/test/testnc.nc"
  cdms_test_4.png
  uvcdat
  0.05
)

#test that clt axis variables can be plot
add_test(cdms_load_and_plot_axis_variable
  ${CMAKE_INSTALL_PREFIX}/bin/runpytest
  ${CMAKE_BINARY_DIR}/tmp
  ${cdat_SOURCE_DIR}/testing/uvcdat/cdms_test_axis_var.vt:test_plot_axis_var
  "datafile=${cdat_SOURCE_DIR}/libcdms/src/cdunif/test/testnc.nc"
  cdms_test_axis_var_5.png
  uvcdat
  0.05
)

#this test verifies that all of the GUI tests pass
add_test(run_uvcdat_gui_tests
  ${CMAKE_INSTALL_PREFIX}/bin/uvcdat 
  -P -N -C 
  -A "${cdat_SOURCE_DIR}" 
  -S ${CMAKE_BINARY_DIR}/tmp
)
