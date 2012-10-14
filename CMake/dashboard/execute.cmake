ctest_start("${PROJECT_BUILD_INTERVAL}")
ctest_update(SOURCE "${CTEST_SOURCE_DIRECTORY}" RETURN_VALUE NUM_FILES_UPDATED)

# Only configure, build, and test for nightlies, new continuous, or on updates
if((NOT "${PROJECT_BUILD_INTERVAL}" STREQUAL "Continuous") OR ("$ENV{FIRST_BUILD}" STREQUAL "TRUE") OR (NUM_FILES_UPDATED GREATER 0) OR ("$ENV{DASHSEND}" STREQUAL "TRUE"))

  ctest_configure()
  ctest_read_custom_files("${CTEST_BINARY_DIRECTORY}")

  ctest_build()
  ctest_test(BUILD "${CTEST_BINARY_DIRECTORY}" PARALLEL_LEVEL 1)

  if(PERFORM_COVERAGE AND CTEST_COVERAGE_COMMAND AND "${PROJECT_BUILD_INTERVAL}" STREQUAL "Nightly")
    ctest_coverage(BUILD "${CTEST_BINARY_DIRECTORY}")
  endif()

  if(PERFORM_MEMCHECK AND CTEST_MEMORYCHECK_COMMAND)
    set(CTEST_MEMORYCHECK_SUPPRESSIONS_FILE "${CTEST_SOURCE_DIRECTORY}/tests/valgrind.supp")
    ctest_memcheck(BUILD "${CTEST_BINARY_DIRECTORY}" PARALLEL_LEVEL 1)
  endif()

  ctest_submit()

endif()
