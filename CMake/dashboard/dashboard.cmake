# Usage: ctest -S dashboard.cmake,"branch_name;build_prefix;target_cmake_file"
# CMake dependencies: common.cmake, execute.cmake

# Provide information about this project
cmake_minimum_required(VERSION 2.8.7)
set(CTEST_PROJECT_NAME "UVCDAT")

# environment variable DASHROOT specifies toplevel dir to checkout and build in
if(NOT DEFINED ENV{DASHROOT})
  message(FATAL_ERROR "[ERROR] DASHROOT environment variable is not set")
  return()
endif()
file(TO_CMAKE_PATH "$ENV{DASHROOT}" DASHROOT)

# Fetch the hostname of the submitting machine
execute_process(COMMAND hostname
  OUTPUT_VARIABLE HOSTNAME
  OUTPUT_STRIP_TRAILING_WHITESPACE)

# get command line arguments
macro(read_args branch_name system_name target_cmake_file)
  set(PROJECT_BRANCH "${branch_name}")
  set(BUILD_NAME "${system_name}")
  set(TARGET_CMAKE_FILE "${target_cmake_file}")
endmacro(read_args)
if(CTEST_SCRIPT_ARG)
  read_args(${CTEST_SCRIPT_ARG})
else()
  #use defaults if not provided
  read_args(master ${CMAKE_SYSTEM_VERSION} "${HOSTNAME}.cmake")
endif()

# this silly dance is because we use separate repos to write protect the master branch
# and use the branch name of master on both the master and devel repos
set(TOPIC_NAME ${PROJECT_BRANCH})
if(PROJECT_BRANCH STREQUAL "master" OR PROJECT_BRANCH STREQUAL "release")
  set(REPO "git://github.com/UV-CDAT/uvcdat.git")
else()
  set(MASTER_REPO "git://github.com/UV-CDAT/uvcdat.git")
  set(REPO "git://github.com/UV-CDAT/uvcdat-devel.git")
  if(PROJECT_BRANCH STREQUAL "next")
    set(TOPIC_NAME "master")
  endif()
endif()

# Display build name and project branch we are currently building
message("[INFO] REPO is ${REPO}")
message("[INFO] BUILD_NAME is ${BUILD_NAME}")
message("[INFO] PROJECT_BRANCH is ${PROJECT_BRANCH}")
message("[INFO] TOPIC_NAME is ${TOPIC_NAME}")

# Detect the processor architecture (should be x86 or X86_64)
set(PROJECT_BUILD_ARCH ${CMAKE_SYSTEM_PROCESSOR})

# Select the dashboard root and type from environment variables
# default to Experimental
set(PROJECT_BUILD_INTERVAL "$ENV{DASHTYPE}")
if(NOT PROJECT_BUILD_INTERVAL MATCHES "^(Nightly|Continuous)$")
  set(PROJECT_BUILD_INTERVAL Experimental)
endif(NOT PROJECT_BUILD_INTERVAL MATCHES "^(Nightly|Continuous)$")

# Provide information about this build
# Provide CTest with some auxilliary information
set(CTEST_CMAKE_COMMAND "\"${CMAKE_EXECUTABLE_NAME}\"")
set(CTEST_NOTES_FILES "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")
find_program(CTEST_GIT_COMMAND NAMES git git.exe git.cmd)
set(CTEST_UPDATE_COMMAND "${CTEST_GIT_COMMAND}")
set(CTEST_BUILD_CONFIGURATION Debug)
set(CTEST_SITE "${HOSTNAME}")
set(CTEST_BUILD_NAME "${CTEST_PROJECT_NAME}-${BUILD_NAME}-${PROJECT_BRANCH}")

set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
set(CTEST_BUILD_COMMAND "/usr/bin/make -j4")

# Dashboards should participate in anonymous logging and must not have a confirmation dialog.
set(ENV{UVCDAT_ANONYMOUS_LOG} "True")

set(PERFORM_MEMCHECK FALSE)
set(PERFORM_COVERAGE FALSE)
find_program(CTEST_MEMCHECK_COMMAND NAMES valgrind)
find_program(CTEST_COVERAGE_COMMAND NAMES gcov44 gcov)
if(CTEST_COVERAGE_COMMAND)
  set(CTEST_COVERAGE_FLAGS "-fprofile-arcs -ftest-coverage")
  #Do NOT set LDflags! Breaks NUMPY build
  #when we want coverage we will need another solution
  #set(ENV{LDFLAGS} "$ENV{LDFLAGS} ${CTEST_COVERAGE_FLAGS}")
endif()

set(CTEST_USE_LAUNCHERS 1)
set(PLATFORM_SPECIFIC_CACHE_DATA "
    CMAKE_INSTALL_PREFIX:PATH=/usr/local
    CMAKE_C_COMPILER:STRING=gcc
    CMAKE_C_FLAGS:STRING=
    CMAKE_CXX_COMPILER:STRING=g++
    CMAKE_CXX_FLAGS:STRING=
    CMAKE_LINKER=/usr/bin/ld --no-copy-dt-needed-entries -z defs --no-undefined --no-allow-shlib-undefined
    CTEST_USE_LAUNCHERS:BOOL=${CTEST_USE_LAUNCHERS}
  ")

set(PROJECT_STATIC_LIB_EXT ".a")

# Set up the source and build directories properly
# Replace slashes with underscores for path
string(REPLACE "/" "_" PROJECT_BRANCH_DIRECTORY "${PROJECT_BRANCH}")
set(CTEST_BINARY_DIRECTORY "${DASHROOT}/${CTEST_PROJECT_NAME}/builds/${PROJECT_BRANCH_DIRECTORY}/UVCDAT-${PROJECT_BUILD_INTERVAL}-${PROJECT_BUILD_ARCH}")
set(CTEST_BINARY_INSTALL_DIRECTORY "${DASHROOT}/${CTEST_PROJECT_NAME}/builds/${PROJECT_BRANCH_DIRECTORY}/install")
set(CTEST_SOURCE_DIRECTORY "${DASHROOT}/${CTEST_PROJECT_NAME}/source/${PROJECT_BRANCH_DIRECTORY}/UVCDAT-${PROJECT_BUILD_INTERVAL}-${PROJECT_BUILD_ARCH}")

# Prepare to do an initial checkout, if necessary
if(CTEST_UPDATE_COMMAND AND NOT EXISTS "${CTEST_SOURCE_DIRECTORY}")
  if(PROJECT_BRANCH STREQUAL "master" OR PROJECT_BRANCH STREQUAL "release")
    set(CTEST_CHECKOUT_COMMAND "${CTEST_SCRIPT_DIRECTORY}/checkout.sh ${CTEST_UPDATE_COMMAND} ${REPO} ${CTEST_SOURCE_DIRECTORY} ${PROJECT_BRANCH}")
  else()
    set(CTEST_CHECKOUT_COMMAND "${CTEST_SCRIPT_DIRECTORY}/next_checkout.sh ${CTEST_UPDATE_COMMAND} ${CTEST_SOURCE_DIRECTORY} ${MASTER_REPO} ${REPO} ${TOPIC_NAME}")
  endif()
endif()

# On non-continuous or first build of the day, clear the build directory
if((NOT "${PROJECT_BUILD_INTERVAL}" STREQUAL "Continuous")
    OR
    ("$ENV{FIRST_BUILD}" STREQUAL "TRUE"))
  file(REMOVE_RECURSE "${CTEST_BINARY_INSTALL_DIRECTORY}")
  ctest_empty_binary_directory("${CTEST_BINARY_DIRECTORY}")
  ctest_empty_binary_directory("${CTEST_BINARY_INSTALL_DIRECTORY}")
endif()

# Prevent pip from using some random directory to do builds in
# Dashboards do it inside the build tree so that builds will be clean each night
set(ENV{TMPDIR} "${CTEST_BINARY_DIRECTORY}")

# Populate CMakeCache using host/target specific settings
if(EXISTS "${CTEST_SCRIPT_DIRECTORY}/${TARGET_CMAKE_FILE}")
  message("[INFO] Including ${CTEST_SCRIPT_DIRECTORY}/${TARGET_CMAKE_FILE}")
  include("${CTEST_SCRIPT_DIRECTORY}/${TARGET_CMAKE_FILE}")
else()
  message(FATAL_ERROR "[ERROR] "${TARGET_CMAKE_FILE}" does not exist")
endif()

# Now that the preliminaries (setup is done) perform actual dashboard execution
ctest_start("${PROJECT_BUILD_INTERVAL}") #nightly | continuous | experimental
ctest_update(SOURCE "${CTEST_SOURCE_DIRECTORY}" RETURN_VALUE NUM_FILES_UPDATED)
message("UPDATED ${NUM_FILES_UPDATED} FILES")

# Only configure, build, and test for nightlies, new continuous, or on updates
if((NOT "${PROJECT_BUILD_INTERVAL}" STREQUAL "Continuous") OR ("$ENV{FIRST_BUILD}" STREQUAL "TRUE") OR (NUM_FILES_UPDATED GREATER 0) OR ("$ENV{DASHSEND}" STREQUAL "TRUE"))

  ctest_configure(OPTIONS "-VV")
  #A second test since cdat doesn't configure in one pass
  ctest_configure(OPTIONS "-VV")

  ctest_read_custom_files("${CTEST_BINARY_DIRECTORY}")

  ctest_build()

  ctest_test(BUILD "${CTEST_BINARY_DIRECTORY}" PARALLEL_LEVEL 1)

  if(PERFORM_COVERAGE AND
      CTEST_COVERAGE_COMMAND AND
      "${PROJECT_BUILD_INTERVAL}" STREQUAL "Nightly")
    ctest_coverage(BUILD "${CTEST_BINARY_DIRECTORY}")
  endif()

  if(PERFORM_MEMCHECK AND CTEST_MEMORYCHECK_COMMAND)
    set(CTEST_MEMORYCHECK_SUPPRESSIONS_FILE
      "${CTEST_SOURCE_DIRECTORY}/tests/valgrind.supp")
    ctest_memcheck(BUILD "${CTEST_BINARY_DIRECTORY}" PARALLEL_LEVEL 1)
  endif()

  ctest_submit()

endif()
