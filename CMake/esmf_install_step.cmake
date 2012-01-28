
include(/home/research/kindig/projects/cdat/CMake/cdat_common_environment.cmake)

set(ENV{ESMF_DIR} /home/research/kindig/projects/cdat/build/esmf)
set(ENV{ESMF_INSTALL_PREFIX} cdat_EXTERNALS})
set(ENV{ESMF_COMM} mpiuni)

execute_process(
  COMMAND gmake install
  WORKING_DIRECTORY /home/research/kindig/projects/cdat/build/esmf
  OUTPUT_VARIABLE CDAT_OUT
  ERROR_VARIABLE CDAT_ERR
  RESULT_VARIABLE res)

if(NOT ${res} EQUAL 0)
  message("Install Errors detected: \n${CDAT_OUT}\n${CDAT_ERR}")
  message(FATAL_ERROR "Error in Install")
endif()
message("Install succeeded.")
