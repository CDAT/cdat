
if (CDAT_DOWNLOAD_UVCMETRICS_TESTDATA)
  set(UVCMETRICS_DOWNLOAD_FILES "")

  file(READ "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/uvcmetrics_test_data_md5s.txt" UVCMETRICS_FILES)
  string(REGEX REPLACE ";" "\\\\;" UVCMETRICS_FILES "${UVCMETRICS_FILES}")
  string(REGEX REPLACE "\n" ";" UVCMETRICS_FILES "${UVCMETRICS_FILES}")

  foreach(line ${UVCMETRICS_FILES})
    string(REGEX REPLACE " +" ";" line "${line}")
    list(GET line 1 base_file_path)
    list(GET line 0 FILE_MD5)

    string(STRIP "${base_file_path}" base_file_path)
    string(STRIP "${FILE_MD5}" FILE_MD5)

    set(FILE_PATH "${UVCMETRICS_TEST_DATA_DIRECTORY}/${base_file_path}")
    list(APPEND UVCMETRICS_DOWNLOAD_FILES "${FILE_PATH}")

    set(FILE_URL "${LLNL_URL}/../sample_data/uvcmetrics/${base_file_path}")

    add_custom_command(
      OUTPUT "${FILE_PATH}"
      COMMAND "${CMAKE_COMMAND}"
        -D FILE_URL="${FILE_URL}"
        -D FILE_MD5="${FILE_MD5}"
        -D FILE_PATH="${FILE_PATH}"
        -P "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/fetch_uvcmetrics_testdata.cmake"
      DEPENDS "${uvcmetrics_data_keyfile}"
      COMMENT "Downloading ${base_file_path}"
    )
  endforeach()

  add_custom_target(uvcmetrics_test_data ALL DEPENDS ${UVCMETRICS_DOWNLOAD_FILES})
endif()

set(GIT_CMD_STR GIT_REPOSITORY "${UVCMETRICS_SOURCE}")
set(GIT_TAG GIT_TAG "${UVCMETRICS_BRANCH}")
set(nm UVCMETRICS)
include(pipinstaller)
