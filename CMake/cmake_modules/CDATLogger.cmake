






message(Source dir ${SOURCE_DIR}"---")

execute_process(
  COMMAND whoami
  COMMAND shasum 
  COMMAND "awk '{print $1}'"
  OUTPUT_VARIABLE whoami_output
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

execute_process(
  COMMAND uname
  OUTPUT_VARIABLE uname_output
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

execute_process(
  COMMAND uname -r
  OUTPUT_VARIABLE uname_r_output
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

execute_process(
  COMMAND uname -n 
  COMMAND shasum 
  COMMAND "awk '{print $1}'"
  OUTPUT_VARIABLE hashed_host_output
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )


execute_process(
  COMMAND ${GIT_EXECUTABLE} log -n 1
  COMMAND grep commit
  COMMAND "awk '{print $2}'"
  WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  OUTPUT_VARIABLE git_log_output
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

find_program(CURL_EXECUTABLE curl)

#if(CURL_EXECUTABLE)
#  execute_process(
#    COMMAND ${CURL_EXECUTABLE} -d "platform=${uname_output}&platform_version=${uname_r_output}&hashed_hostname=${hashed_host_output}&source=CMAKE&source_version=${git_log_output}&action=install&hashed_username=${whoami_output}" http://uv-cdat.llnl.gov/UVCDATUsage/log/add 
#  )
#endif()
