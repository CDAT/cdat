set(CTEST_SOURCE_DIRECTORY "$ENV{TRAVIS_BUILD_DIR}")
set(CTEST_BINARY_DIRECTORY "$ENV{TRAVIS_BUILD_DIR}/../_build")

include(${CTEST_SOURCE_DIRECTORY}/CTestConfig.cmake)
set(CTEST_SITE "Travis")
set(CTEST_BUILD_NAME "Linux-$ENV{TRAVIS_BRANCH}")
set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
set (CTEST_ENVIRONMENT
   "PATH=$ENV{TRAVIS_BUILD_DIR}/../_build/install/bin:$ENV{TRAVIS_BUILD_DIR}/../_build/install/External/bin"
   "LD_LIBRARY_PATH=$ENV{TRAVIS_BUILD_DIR}/../_build/install/lib:$ENV{TRAVIS_BUILD_DIR}/../_build/install/External/lib"
   "DYLD_LIBRARY_PATH=$ENV{TRAVIS_BUILD_DIR}/../_build/install/lib:$ENV{TRAVIS_BUILD_DIR}/../_build/install/External/lib"
   "PYTHONPATH=$ENV{TRAVIS_BUILD_DIR}/../_build/install/lib/python2.7/site-packages:$ENV{TRAVIS_BUILD_DIR}/../_build/install/Externals/lib/python2.7/site-packages"
)
set(CTEST_COMMAND "${CTEST_EXECUTABLE_NAME} -j4")

ctest_start("Continuous")
ctest_submit()
