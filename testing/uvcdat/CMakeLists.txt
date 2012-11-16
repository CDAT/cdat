add_test(run_uvcdat ${CMAKE_INSTALL_PREFIX}/bin/uvcdat -P -T 2)


add_test(cdms_test ${CMAKE_INSTALL_PREFIX}/bin/uvcdat -b ${cdat_SOURCE_DIR}/testing/uvcdat/cdmf_test.vt:test -a "datafile=${cdat_SOURCE_DIR}/libcdms/src/cdunif/test/testnc.nc")
