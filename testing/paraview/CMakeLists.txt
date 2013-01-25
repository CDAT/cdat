set (sampledata_path "${CMAKE_INSTALL_PREFIX}/sample_data")
if (APPLE)
  set (sampledata_path "${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/2.7/sample_data")
endif()

#test uvcdat's paraview contour representation
add_test(paraview_contour
  ${CMAKE_INSTALL_PREFIX}/bin/runpytest
  ${CMAKE_BINARY_DIR}/tmp
  ${cdat_SOURCE_DIR}/testing/paraview/testcontour.vt:pvcontour_example
  "filename=${sampledata_path}/clt.nc"
  testcontour_5.png
  paraview
  0.05
)

#test uvcdat's paraview slice representation
add_test(paraview_slice
  ${CMAKE_INSTALL_PREFIX}/bin/runpytest
  ${CMAKE_BINARY_DIR}/tmp
  ${cdat_SOURCE_DIR}/testing/paraview/testslice.vt:demonstrat_PVslice
  "datafile=${sampledata_path}/clt.nc"
  testslice_4.png
  paraview
  0.05
)
