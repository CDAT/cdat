# Usage:
# cmake -DGIT_EXECUTABLE=[git executable]
#       -DTESTDATA_URL=[uvcdat-testdata url]
#       -DTESTDATA_DIR=[local testdata directory]
#       -P checkout_testdata.cmake
#
# This script creates and syncs a clone of the uvcdat-testdata directory.
#
# In detail:
#
# 1) Check if the TESTDATA_DIR exists.
#    If not, clone the repo and exit.
# 2) Check if the TESTDATA_DIR is a git repo with TESTDATA_URL as its origin.
#    If not, abort with a warning message.
# 3) Check if the TESTDATA_DIR repo is checked out to master.
#    If not, abort with an warning message.
# 4) Check if the TESTDATA_DIR repo is clean.
#    If not, abort with an warning message.
# 5) Run 'git pull origin master:master' to update the repository.
#
# Any failures are handled via non-fatal warnings. This is to allow the project
# to build when access to the repo is not available.

# 1) Clone and exit if the target directory doesn't exist.
if(NOT EXISTS "${TESTDATA_DIR}")
  message("Cloning \"${TESTDATA_URL}\" into \"${TESTDATA_DIR}\"...")

  # Use depth=1 to avoid fetching the full history. Use "git pull --unshallow"
  # to backfill the history if needed.
  execute_process(COMMAND
    "${GIT_EXECUTABLE}" clone --depth=1 "${TESTDATA_URL}" "${TESTDATA_DIR}"
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE OUTPUT
    OUTPUT_VARIABLE OUTPUT)

  string(STRIP "${OUTPUT}" OUTPUT)

  message("${OUTPUT}")

  if(NOT RESULT EQUAL 0)
    message("Could not clone test data repo! "
            "Baseline images will not be available.")
  endif()

  return()
endif()

# 2) Is TESTDATA_DIR a clone of TESTDATA_URL?
execute_process(COMMAND
  "${GIT_EXECUTABLE}" config --get remote.origin.url
  WORKING_DIRECTORY "${TESTDATA_DIR}"
  RESULT_VARIABLE RESULT
  ERROR_VARIABLE OUTPUT
  OUTPUT_VARIABLE OUTPUT)

if(NOT RESULT EQUAL 0)
  message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
          "Directory exists and is not a git repository. "
          "Baseline images may be out of date.")
  return()
endif()

string(STRIP "${OUTPUT}" OUTPUT)

if(NOT "${TESTDATA_URL}" STREQUAL "${OUTPUT}")
  message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
          "Directory is a git clone of \"${OUTPUT}\", not \"${TESTDATA_URL}\". "
          "Baseline images may be out of date.")
  return()
endif()

# 3) Are we on the master branch?
execute_process(COMMAND
  "${GIT_EXECUTABLE}" rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY "${TESTDATA_DIR}"
  RESULT_VARIABLE RESULT
  ERROR_VARIABLE OUTPUT
  OUTPUT_VARIABLE OUTPUT)

if(NOT RESULT EQUAL 0)
  message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
          "Cannot determine current branch name. "
          "Baseline images may be out of date.")
  return()
endif()

string(STRIP "${OUTPUT}" OUTPUT)

if(NOT "${OUTPUT}" STREQUAL "master")
  message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
          "Current branch is not master, but rather \"${OUTPUT}\". "
          "Baseline images may be out of date.")
  return()
endif()

# 4) Is the repo clean?
# Update the index first:
execute_process(COMMAND
  "${GIT_EXECUTABLE}" update-index -q --refresh
  WORKING_DIRECTORY "${TESTDATA_DIR}"
  RESULT_VARIABLE RESULT
  ERROR_VARIABLE OUTPUT
  OUTPUT_VARIABLE OUTPUT)

if(NOT RESULT EQUAL 0)
  message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
          "Error updating current index with 'git update-index -q --refresh':\n."
          "${OUTPUT}\n"
          "Baseline images may be out of date.")
  return()
endif()

# Now check if the index is dirty:
execute_process(COMMAND
  "${GIT_EXECUTABLE}" diff-index --name-only HEAD "--"
  WORKING_DIRECTORY "${TESTDATA_DIR}"
  RESULT_VARIABLE RESULT
  ERROR_VARIABLE OUTPUT
  OUTPUT_VARIABLE OUTPUT)

if(NOT RESULT EQUAL 0)
  message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
          "Error checking current index with 'git diff-index --name-only HEAD --':\n."
          "${OUTPUT}\n"
          "Baseline images may be out of date.")
  return()
endif()

string(STRIP "${OUTPUT}" OUTPUT)

if(NOT "${OUTPUT}" STREQUAL "")
  message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
          "Current checkout is not clean. The following files have modifications:\n"
          "${OUTPUT}\n"
          "Baseline images may be out of date.")
  return()
endif()

# 5) Update the repo:
message("Updating \"${TESTDATA_DIR}:master\" from \"${TESTDATA_URL}:master\"...")
execute_process(COMMAND
  "${GIT_EXECUTABLE}" pull origin master:master
  WORKING_DIRECTORY "${TESTDATA_DIR}"
  RESULT_VARIABLE RESULT
  ERROR_VARIABLE OUTPUT
  OUTPUT_VARIABLE OUTPUT)

string(STRIP "${OUTPUT}" OUTPUT)

message("${OUTPUT}")

if(NOT RESULT EQUAL 0)
  message("Error updating testdata repo! "
          "Baseline images may be out of date.")
endif()
