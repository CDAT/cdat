# Usage:
# cmake -DGIT_EXECUTABLE=[git executable]
#       -DTESTDATA_URL=[uvcdat-testdata url]
#       -DTESTDATA_DIR=[local testdata directory]
#       -DSOURCE_DIR=[uvcdat source root]
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
# 3) Check if the TESTDATA_DIR repo is clean.
#    If not, abort with an warning message.
# 4) Fetch the current git branch name for the SOURCE_DIR repo.
#    If the current HEAD is not a named branch, use master.
# 5) Update the remote branches in the TESTDATA_DIR repo.
# 6) Check if the desired branch exists in TESTDATA_DIR's origin remote.
# 7) Check if the desired branch exists in TESTDATA_DIR as a local branch.
# 8) If the neither the local or remote branch exist, use master.
# 9) Check out the local <branch> in TESTDATA_DIR repo.
# 10) If the remote branch exists, or we are using master, run
#     'git pull origin <branch>:<branch>' to fetch/update the local branch from
#     the remote.
#
# Any failures are handled via non-fatal warnings. This is to allow the project
# to build when access to the repo is not available.

# 1) Clone and exit if the target directory doesn't exist.
if(NOT EXISTS "${TESTDATA_DIR}")
  message("Cloning \"${TESTDATA_URL}\" into \"${TESTDATA_DIR}\"...")
  message("Cloning COMMAND: ${GIT_EXECUTABLE} clone --depth=1 --no-single-branch ${TESTDATA_URL} ${TESTDATA_DIR}")

  # Use depth=1 to avoid fetching the full history. Use "git pull --unshallow"
  # to backfill the history if needed.
  # --no-single-branch fetches the tip of all remote branches -- this is needed
  # for auto-updating the testdata when the source branch changes.
  execute_process(COMMAND
    "${GIT_EXECUTABLE}"
      clone --depth=1 --no-single-branch "${TESTDATA_URL}" "${TESTDATA_DIR}"
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE OUTPUT
    OUTPUT_VARIABLE OUTPUT)

  string(STRIP "${OUTPUT}" OUTPUT)

  message("${OUTPUT}")

  if(NOT RESULT EQUAL 0)
    message("Could not clone test data repo! "
            "Baseline images will not be available.")
    return()
  endif()
endif()

# bots merge master in and mess the following, always rechecking master
# bots check out the correct branches
# following keyword skips the branch checking
if (CDAT_CHECKOUT_BASELINE_MATCHING_BRANCH)
    message("[INFO] Trying to find matching branch on baseline repo")
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

    # 3) Is the current testdata repo clean? Don't want to clobber any local mods.
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

    # 4) Get the current branch name of the source repo.
    execute_process(COMMAND
      "${GIT_EXECUTABLE}" rev-parse --abbrev-ref HEAD
      WORKING_DIRECTORY "${SOURCE_DIR}"
      RESULT_VARIABLE RESULT
      ERROR_VARIABLE OUTPUT
      OUTPUT_VARIABLE OUTPUT)

    if(NOT RESULT EQUAL 0)
      message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
              "Cannot determine current branch name of source directory. "
              "Baseline images may be out of date.")
      return()
    endif()

    string(STRIP "${OUTPUT}" BRANCH)

    # If BRANCH is "HEAD", we're not on a named branch. Just use master in that
    # case.
    if("${BRANCH}" STREQUAL "HEAD")
      message("The current source directory at '${SOURCE_DIR}' is not on a named "
              "branch. Using the 'master' branch of the testdata repo.")
      set(BRANCH "master")
    endif()

    # 5) Update the remote branches available on the testdata repo.
    execute_process(COMMAND
      "${GIT_EXECUTABLE}" fetch --depth=1
      WORKING_DIRECTORY "${TESTDATA_DIR}"
      RESULT_VARIABLE RESULT
      ERROR_VARIABLE OUTPUT
      OUTPUT_VARIABLE OUTPUT)

    if(NOT RESULT EQUAL 0)
      message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
              "Error updating remote branches with "
              "'git fetch --depth=1':\n."
              "${OUTPUT}\n"
              "Baseline images may be out of date.")
      return()
    endif()

    # 6) Check if the desired branch exists in TESTDATA_DIR's origin remote.
    execute_process(COMMAND
      "${GIT_EXECUTABLE}" branch -a --list "*${BRANCH}"
      WORKING_DIRECTORY "${TESTDATA_DIR}"
      RESULT_VARIABLE RESULT
      ERROR_VARIABLE OUTPUT
      OUTPUT_VARIABLE OUTPUT)

    if(NOT RESULT EQUAL 0)
      message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
              "Error obtaining full branch list:\n${OUTPUT}"
              "Baseline images may be out of date.")
      return()
    endif()

    message("Testing if remote branch 'origin/${BRANCH}' exists...")
    string(FIND "${OUTPUT}" " remotes/origin/${BRANCH}\n" POS)
    if(NOT POS EQUAL -1)
      message("Remote branch exists.")
      set(REMOTE_EXISTS "YES")
    else()
      message("Remote branch does not exist.")
      set(REMOTE_EXISTS "NO")
    endif()

    # 7) Check if the desired branch exists locally:
    message("Testing if local branch '${BRANCH}' exists...")
    string(FIND "${OUTPUT}" " ${BRANCH}\n" POS) # Leading space in regex intended
    if(NOT POS EQUAL -1)
      message("Local branch exists.")
      set(LOCAL_EXISTS "YES")
    else()
      message("Local branch does not exist.")
      set(LOCAL_EXISTS "NO")
    endif()

    # 8) If the neither the local or remote branch exist, use master.
    if(NOT REMOTE_EXISTS AND NOT LOCAL_EXISTS)
      set(BRANCH "master")
      set(REMOTE_EXISTS "YES")
      set(LOCAL_EXISTS "YES")
    endif()

    # 9) Check out the desired branch in TESTDATA_DIR repo.
    message("Checking out branch '${BRANCH}' in repo '${TESTDATA_DIR}'.")
    execute_process(COMMAND
      "${GIT_EXECUTABLE}" checkout "${BRANCH}"
      WORKING_DIRECTORY "${TESTDATA_DIR}"
      RESULT_VARIABLE RESULT
      ERROR_VARIABLE OUTPUT
      OUTPUT_VARIABLE OUTPUT)

    if(NOT RESULT EQUAL 0)
      message("Cannot update uvcdat-testdata checkout at \"${TESTDATA_DIR}\". "
              "Error executing 'git checkout ${BRANCH}':\n."
              "${OUTPUT}\n"
              "Baseline images may be out of date.")
      return()
    endif()

    # 10) If the remote branch exists, or we are using master, run
    #     'git pull origin <branch>:<branch>' to fetch/update the local branch from
    #     the remote.
    if(REMOTE_EXISTS)
      message("Updating \"${TESTDATA_DIR}:${BRANCH}\" from "
              "\"${TESTDATA_URL}:${BRANCH}\"...")
      execute_process(COMMAND
        "${GIT_EXECUTABLE}" pull origin "${BRANCH}:${BRANCH}"
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
    endif()
else()
    message("[INFO] NOT trying to switch branch on baseline (only bots should turn this on)")
endif()
