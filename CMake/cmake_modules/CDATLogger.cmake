

execute_process(
  COMMAND whoami
  OUTPUT_VARIABLE whoami_output
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

execute_process(
  COMMAND uname
  OUTPUT_VARIABLE uname_output
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

execute_process(
  COMMAND git log -n 1 --pretty=format:%H
  WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  OUTPUT_VARIABLE git_log_output
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

find_program(CURL_EXECUTABLE curl)

if(CURL_EXECUTABLE)
  execute_process(
    COMMAND ${CURL_EXECUTABLE} http://uv-cdat.llnl.gov/UVCDATLogger/${whoami_output}/${uname_output}/bldcmk/${git_log_output}
  )
endif()
