cmake_minimum_required(VERSION 2.8.7)

# Requires DASHROOT to be set
if(NOT DEFINED ENV{DASHROOT})
  message(FATAL_ERROR "[ERROR] DASHROOT environment variable is not set")
endif()

file(TO_CMAKE_PATH "$ENV{DASHROOT}" DASHROOT)

# Fetch the hostname of the current machine (assumes at kitware)
execute_process(COMMAND hostname OUTPUT_VARIABLE HOSTNAME OUTPUT_STRIP_TRAILING_WHITESPACE)

# Helpful macro
macro(read_args branch_name system_name target_cmake_file)
  set(PROJECT_BRANCH "${branch_name}")
  set(BUILD_NAME "${system_name}")
  set(TARGET_CMAKE_FILE "${target_cmake_file}")
endmacro(read_args)

# Get the branch name from the script argument, default to master
if(CTEST_SCRIPT_ARG)
  read_args(${CTEST_SCRIPT_ARG})
else(CTEST_SCRIPT_ARG)
  read_args(master ${CMAKE_SYSTEM_VERSION} "${HOSTNAME}.cmake")
endif(CTEST_SCRIPT_ARG)

# Display build name and project branch we are currently building
message("[INFO] BUILD_NAME is ${BUILD_NAME}")
message("[INFO] PROJECT_BRANCH is ${PROJECT_BRANCH}")

# Detect the processor architecture (should be x86 or X86_64)
set(PROJECT_BUILD_ARCH ${CMAKE_SYSTEM_PROCESSOR})

# Select the dashboard root and type from environment variables, default to Experimental
set(PROJECT_BUILD_INTERVAL "$ENV{DASHTYPE}")
if(NOT PROJECT_BUILD_INTERVAL MATCHES "^(Nightly|Continuous)$")
  set(PROJECT_BUILD_INTERVAL Experimental)
endif(NOT PROJECT_BUILD_INTERVAL MATCHES "^(Nightly|Continuous)$")

# For dynamic analysis and coverage tests (made optional)
set(PERFORM_MEMCHECK TRUE)
set(PERFORM_COVERAGE TRUE)

# Provide information about this build
# Provide CTest with some auxilliary information
set(CTEST_CMAKE_COMMAND "\"${CMAKE_EXECUTABLE_NAME}\"")
set(CTEST_NOTES_FILES "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")
find_program(CTEST_GIT_COMMAND NAMES git git.exe git.cmd)
set(CTEST_UPDATE_COMMAND "${CTEST_GIT_COMMAND}")
set(CTEST_BUILD_CONFIGURATION Debug)
set(CTEST_SITE "${HOSTNAME}.kitware")
set(CTEST_BUILD_NAME "${BUILD_NAME}-${PROJECT_BUILD_ARCH}-${PROJECT_BRANCH}")
