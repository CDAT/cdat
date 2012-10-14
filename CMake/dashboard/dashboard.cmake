# Usage: ctest -S dashboard.cmake,"branch_name;build_prefix;target_cmake_file"
# CMake dependencies: common.cmake, execute.cmake

# Provide information about this project
cmake_minimum_required(VERSION 2.8.7)
set(CTEST_PROJECT_NAME "UVCDAT")

include("${CTEST_SCRIPT_DIRECTORY}/setup.cmake")

# Set specific properties dependent on build platform (unix/w32/w64)
if(UNIX)

  set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
  set(CTEST_BUILD_COMMAND "/usr/bin/make -j9 -k")

  find_program(CTEST_MEMCHECK_COMMAND NAMES valgrind)
  find_program(CTEST_COVERAGE_COMMAND NAMES gcov44 gcov)

  if(CTEST_COVERAGE_COMMAND)
    set(CTEST_COVERAGE_FLAGS "-fprofile-arcs -ftest-coverage")
    set(ENV{LDFLAGS} "$ENV{LDFLAGS} ${CTEST_COVERAGE_FLAGS}")
  endif()

  set(CTEST_USE_LAUNCHERS 1)
  set(PLATFORM_SPECIFIC_CACHE_DATA "
    CMAKE_INSTALL_PREFIX:PATH=/usr/local
    CMAKE_C_COMPILER:STRING=gcc
    CMAKE_C_FLAGS:STRING=-Wall -Wextra -Wcast-qual ${CTEST_COVERAGE_FLAGS}
    CMAKE_CXX_COMPILER:STRING=g++
    CMAKE_CXX_FLAGS:STRING=-Wall -Wextra -Wunused -Wcast-qual -Wnon-virtual-dtor -Werror=init-self -Werror=reorder ${CTEST_COVERAGE_FLAGS}
    CMAKE_LINKER=/usr/bin/ld --no-copy-dt-needed-entries -z defs --no-undefined --no-allow-shlib-undefined
    CTEST_USE_LAUNCHERS:BOOL=${CTEST_USE_LAUNCHERS}
  ")

  set(PROJECT_STATIC_LIB_EXT ".a")

elseif(WIN32)

  # Windows machines don't have valgrind or gcov
  set(PERFORM_MEMCHECK FALSE)
  set(PERFORM_COVERAGE FALSE)

  # Select Win64 if applicable (isn't a 32-bit environment, or 64-bit forced)
  string(COMPARE NOTEQUAL "$ENV{WIN32DIR}" "" USE_32_BIT)
  string(COMPARE NOTEQUAL "$ENV{FORCE64BIT}" "" FORCE_64_BIT)

  if((USE_32_BIT OR ("${PROJECT_BUILD_ARCH}" STREQUAL "x86")) AND NOT FORCE_64_BIT)
    set(CTEST_CMAKE_GENERATOR "Visual Studio 9 2008")
    set(PROJECT_BUILD_ARCH "x86")
  else((USE_32_BIT OR ("${PROJECT_BUILD_ARCH}" STREQUAL "x86")) AND NOT FORCE_64_BIT)
    set(CTEST_CMAKE_GENERATOR "Visual Studio 9 2008 Win64")
    set(PROJECT_BUILD_ARCH "x86_64")
  endif()

  set(PLATFORM_SPECIFIC_CACHE_DATA "
    CMAKE_C_FLAGS:STRING=/DWIN32 /D_WINDOWS /W3 /Zm1000 /MP
    CMAKE_C_FLAGS_RELEASE:STRING=/Ox
    CMAKE_CXX_FLAGS:STRING=/DWIN32 /D_WINDOWS /W3 /Zm1000 /EHsc /GR /MP
    CMAKE_CXX_FLAGS_RELEASE:STRING=/Ox
    GITCOMMAND:FILEPATH=${CTEST_GIT_COMMAND}
    git_command:FILEPATH=${CTEST_GIT_COMMAND}
  ")
  set(PROJECT_STATIC_LIB_EXT ".lib")
endif()

# Set up the source and build directories properly
string(REPLACE "/" "_" PROJECT_BRANCH_DIRECTORY "${PROJECT_BRANCH}") # Replace slashes with underscores for path
set(CTEST_BINARY_DIRECTORY "${DASHROOT}/${CTEST_PROJECT_NAME}/builds/${PROJECT_BRANCH_DIRECTORY}/UVCDAT-${PROJECT_BUILD_INTERVAL}-${PROJECT_BUILD_ARCH}")
set(CTEST_SOURCE_DIRECTORY "${DASHROOT}/${CTEST_PROJECT_NAME}/source/${PROJECT_BRANCH_DIRECTORY}/UVCDAT-${PROJECT_BUILD_INTERVAL}-${PROJECT_BUILD_ARCH}")

# On non-continuous or first build of the day, clear the build directory
if((NOT "${PROJECT_BUILD_INTERVAL}" STREQUAL "Continuous") OR ("$ENV{FIRST_BUILD}" STREQUAL "TRUE"))
  ctest_empty_binary_directory("${CTEST_BINARY_DIRECTORY}")
endif()

# Populate CMakeCache using host/target specific settings
if(EXISTS "${CTEST_SCRIPT_DIRECTORY}/${TARGET_CMAKE_FILE}")
  message("[INFO] Including ${CTEST_SCRIPT_DIRECTORY}/${TARGET_CMAKE_FILE}")
  include("${CTEST_SCRIPT_DIRECTORY}/${TARGET_CMAKE_FILE}")
else()
  message(FATAL_ERROR "[ERROR] "${TARGET_CMAKE_FILE}" does not exist")
endif()

# Prepare to do an initial checkout, if necessary
if(CTEST_UPDATE_COMMAND AND NOT EXISTS "${CTEST_SOURCE_DIRECTORY}")
  set(CTEST_CHECKOUT_COMMAND "${CTEST_UPDATE_COMMAND} clone -b ${PROJECT_BRANCH} git://uv-cdat.llnl.gov/uv-cdat.git ${CTEST_SOURCE_DIRECTORY}")
endif()

# Perform actual dashboard execution
include("${CTEST_SCRIPT_DIRECTORY}/execute.cmake")
