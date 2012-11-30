#this test verifies that uvcdat starts up and exits cleanly
add_test(run_uvcdat ${CMAKE_INSTALL_PREFIX}/bin/uvcdat -P -T 2 -S ${CMAKE_BINARY_DIR}/tmp)

#this test verifies that we can open a file and show it
#we do not yet do any image comparison so we aren't testing that it looks right
add_test(cdms_test
  ${CMAKE_INSTALL_PREFIX}/bin/uvcdat
  -S ${CMAKE_BINARY_DIR}/tmp
  -b ${cdat_SOURCE_DIR}/testing/uvcdat/cdmf_test.vt:test
  -a "datafile=${cdat_SOURCE_DIR}/libcdms/src/cdunif/test/testnc.nc")
