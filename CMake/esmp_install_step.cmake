
include(/home/research/kindig/projects/cdat/CMake/cdat_common_environment.cmake)

message("")
message("Install_step ESMP")
message("source "/home/research/kindig/projects/cdat/build/ESMP)
message(/tmp/cdat/Externals)
message("install "/tmp/cdat/lib/python2.7/site-packages/)
message("")

message("INSTALL /home/research/kindig/projects/cdat/build/ESMP DESTINATION /tmp/cdat/lib/python2.7/site-packages/")
file(INSTALL /home/research/kindig/projects/cdat/build/ESMP DESTINATION /tmp/cdat/lib/python2.7/site-packages/)

if(NOT ${res} EQUAL 0)
  message("Install Errors detected: \n${CDAT_OUT}\n${CDAT_ERR}")
  message(FATAL_ERROR "Error in Install")
endif()
message("Install succeeded.")
