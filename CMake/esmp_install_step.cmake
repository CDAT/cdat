
include(/home/research/kindig/projects/cdat/CMake/cdat_common_environment.cmake)

set(esmp_source "${CMAKE_CURRENT_BINARY_DIR}/")

#set(PYTHON_VER python${PYTHON_MAJOR}.${PYTHON_MINOR})
set(PYTHON_VER python2.7)

set(PYTHON_DEST /tmp/cdat/lib/${PYTHON_VER}/site-packages/ESMP)
message("dest " ${PYTHON_DEST})
file(INSTALL ${esmp_source}/ DESTINATION ${PYTHON_DEST})

set(res 0)
if(NOT ${res} EQUAL 0)
  message("Install Errors detected: \n${CDAT_OUT}\n${CDAT_ERR}")
  message(FATAL_ERROR "Error in Install")
endif()
message("Install succeeded.")
