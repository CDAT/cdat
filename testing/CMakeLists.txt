set(PYTHON_EXECUTABLE python)
set(CDAT_DOWNLOAD_SAMPLE_DATA ON)

# Helper macro that sets the environment correctly
macro (cdat_add_test name)
  #string (REPLACE ";" " " TEST_ARGS "${ARGN}")
  set(ARGS ${ARGN})
  if(CDAT_MEASURE_COVERAGE)
    if(${ARGV1} STREQUAL ${PYTHON_EXECUTABLE})
      list(REMOVE_AT ARGS 0)
      list(INSERT ARGS 0 "coverage run --parallel-mode --source=${COVERAGE_PKGS}")
    endif()
  endif()

  add_test(${name} ${cdat_CMAKE_BINARY_DIR}/runtest  ${ARGS})
  if ( NOT (${name} STREQUAL download_sample_data ))
      set_tests_properties(${name} PROPERTIES DEPENDS download_sample_data)
  endif()

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

#separate_arguments(DOWNLOAD_ARGS)
# make sure data is downloaded
cdat_add_test(download_sample_data
    vcs_download_sample_data
    )

add_subdirectory(regrid)
add_subdirectory(vcs)
add_subdirectory(vcsaddons)
add_subdirectory(dv3d)
add_subdirectory(cdutil)
add_subdirectory(Thermo)
add_subdirectory(unidata)
add_subdirectory(cdms2)
add_subdirectory(xmgrace)
add_subdirectory(pcmdi)
