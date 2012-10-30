if(CDAT_BUILD_PARAVIEW OR CDAT_USE_SYSTEM_PARAVIEW)
  add_cdat_package(PVFileDialog "" "Build ParaView File Dialog" ON)
endif()

set(PVFileDialog_deps ${paraview_pkg} ${sip_pkg})
