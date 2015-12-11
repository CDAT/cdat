# Disabling GUI tests as they don't work
#add_subdirectory(uvcdat)

# Helper macro that sets the environment correctly
macro (cdat_add_test name)
  #string (REPLACE ";" " " TEST_ARGS "${ARGN}")
  add_test(${name} "${CMAKE_INSTALL_PREFIX}/bin/runtest"
           ${ARGN})
  if(DEFINED ENV{UVCDAT_ANONYMOUS_LOG})
    set_tests_properties (${name}
      PROPERTIES ENVIRONMENT "UVCDAT_ANONYMOUS_LOG=$ENV{UVCDAT_ANONYMOUS_LOG}"
    )
  else()
    set_tests_properties (${name}
      PROPERTIES ENVIRONMENT "UVCDAT_ANONYMOUS_LOG=no"
    )
  endif()
endmacro()
if (CDAT_BUILD_GRAPHICS)
  add_subdirectory(regrid)
  add_subdirectory(vcs)
  add_subdirectory(vcsaddons)
  add_subdirectory(dv3d)
endif()
add_subdirectory(cdutil)
add_subdirectory(Thermo)
add_subdirectory(unidata)
add_subdirectory(cdms2)
add_subdirectory(xmgrace)
add_subdirectory(Thermo)
if (CDAT_BUILD_UVCMETRICSPKG)
  add_subdirectory(metrics)
endif()

# Disabling ParaView tests
#if (CDAT_BUILD_PARAVIEW)
#  add_subdirectory(paraview)
#endif()

# Test RPY2
if (CDAT_BUILD_RPY2)
  add_subdirectory(rpy2)
endif()

# Test Matplotlib
if (CDAT_BUILD_MATPLOTLIB)
  add_subdirectory(matplotlib)
endif()

# CMake module tests:
# Test that out-of-source build detection is working:
add_test(cmake_checkBuildOutOfSource
  "${CMAKE_COMMAND}"
    -DTEST_check_build_out_of_source=ON
    -P "${cdat_SOURCE_DIR}/CMake/cmake_modules/CheckBuildOutOfSource.cmake"
)
