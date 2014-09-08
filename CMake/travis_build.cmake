set(CTEST_SOURCE_DIRECTORY "$ENV{TRAVIS_BUILD_DIR}")
set(CTEST_BINARY_DIRECTORY "$ENV{TRAVIS_BUILD_DIR}/../_build")

include(${CTEST_SOURCE_DIRECTORY}/CTestConfig.cmake)
set(CTEST_SITE "Travis")
set(CTEST_BUILD_NAME "Linux-$ENV{TRAVIS_BRANCH}")
set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
set(PATH "${CTEST_BINARY_DIRECTORY}/install/bin:${CTEST_BINARY_DIRECTORY}/install/Externals/bin")
set(LD_LIBRARY_PATH "${CTEST_BINARY_DIRECTORY}/install/lib:${CTEST_BINARY_DIRECTORY}/install/Externals/lib")
set(DYLD_LIBRARY_PATH "${CTEST_BINARY_DIRECTORY}/install/lib:${CTEST_BINARY_DIRECTORY}/install/Externals/lib")
set(PYTHONPATH "${CTEST_BINARY_DIRECTORY}/install/lib/python2.7/site-packages:${CTEST_BINARY_DIRECTORY}/install/Externals/lib/python2.7/site-packages")

set(CTEST_ENVIRONMENT
   "PATH=${PATH}"
   "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}"
   "DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}"
   "PYTHONPATH=${PYTHONPATH}")
set(CTEST_COMMAND "${CTEST_EXECUTABLE_NAME} -j4")
message("CTEST_ENVIRONMENT ${CTEST_ENVIRONMENT}")

ctest_start("Continuous")
ctest_configure()
ctest_build()
ctest_test(INCLUDE vcs PARALLEL_LEVEL 1 RETURN_VALUE res)
#ctest_coverage()
#file(REMOVE ${CTEST_BINARY_DIRECTORY}/coverage.xml)

if(NOT res EQUAL 0)
  message(FATAL_ERROR "Test failures occurred.")
endif()
