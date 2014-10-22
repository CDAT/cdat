set (sampledata_path "${CMAKE_INSTALL_PREFIX}/sample_data")
if (APPLE)
  set (sampledata_path "${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/2.7/sample_data")
endif()

set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/paraview")

#test uvcdat's paraview contour representation
if(CDAT_DOWNLOAD_SAMPLE_DATA)
  add_test(uvcdat_paraview_contour_plot
    ${CMAKE_INSTALL_PREFIX}/bin/runpytest
    ${CMAKE_BINARY_DIR}/tmp
    ${cdat_SOURCE_DIR}/testing/paraview/testcontour.vt:pvcontour_example
    "filename=${sampledata_path}/clt.nc"
    "${BASELINE_DIR}/testcontour_5.png"
    paraview
    0.05
    )

  #test uvcdat's paraview slice representation
  add_test(uvcdat_paraview_slice_plot
    ${CMAKE_INSTALL_PREFIX}/bin/runpytest
    ${CMAKE_BINARY_DIR}/tmp
    ${cdat_SOURCE_DIR}/testing/paraview/testslice.vt:demonstrat_PVslice
    "datafile=${sampledata_path}/clt.nc"
    "${BASELINE_DIR}/testslice_4.png"
    paraview
    0.05
    )
endif()

add_test(NAME ParaViewSpatioTemporalStatistics
  COMMAND ${CMAKE_CTEST_COMMAND} -R TestMultiBlockTemporalStatistics
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/build/ParaView-build)

# ParaView specific tests that rely on POP test data
if(UVCDAT_TESTDATA_LOCATION)
  add_test(NAME ParaViewMHTReader
    COMMAND ${CMAKE_CTEST_COMMAND} -R TestMHTReader
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/build/ParaView-build)

  add_test(NAME ParaViewMOCReader
    COMMAND ${CMAKE_CTEST_COMMAND} -R TestMOCReader
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/build/ParaView-build)
endif()
