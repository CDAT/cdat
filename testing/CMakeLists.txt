# Disabling GUI tests as they don't work
#add_subdirectory(uvcdat)

add_subdirectory(regrid)
if (CDAT_BUILD_VCS_LEGACY)
  add_subdirectory(vcs_legacy)
endif()
add_subdirectory(vcs)
add_subdirectory(vcsaddons)
add_subdirectory(dv3d)
add_subdirectory(cdutil)
add_subdirectory(cdms2)
if (CDAT_BUILD_UVCMETRICSPKG)
  add_subdirectory(metrics)
endif()

# Disabling ParaView tests
#if (CDAT_BUILD_PARAVIEW)
#  add_subdirectory(paraview)
#endif()

# CMake module tests:
# Test that out-of-source build detection is working:
add_test(cmake_checkBuildOutOfSource
  "${CMAKE_COMMAND}"
    -DTEST_check_build_out_of_source=ON
    -P "${cdat_SOURCE_DIR}/CMake/cmake_modules/CheckBuildOutOfSource.cmake"
)
