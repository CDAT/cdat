# - FindSciDoxygen: Module to find doxygen and setup apidocs target for
#   Doxygen.
#
# Module usage:
#   find_package(SciDoxygen ...)
#
# This module will define the following variables:
#  DOXYGEN_FOUND         = Whether Doxygen was found
#  DOXYGEN_EXECUTABLE    = Path to doxygen executable

######################################################################
#
# SciDoxygen: Find Doxygen and set up apidocs target
#
# $Id: FindSciDoxygen.cmake 1161 2011-12-17 15:44:00Z cary $
#
# Copyright 2011 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

message("")
message("--------- FindSciDoxygen looking for doxygen ---------")
find_package(Doxygen)
if (DOXYGEN_FOUND)
  message(STATUS "DOXYGEN_EXECUTABLE found.")
  message(STATUS "DOXYGEN_EXECUTABLE = ${DOXYGEN_EXECUTABLE}")
else ()
  message(STATUS "DOXYGEN_EXECUTABLE not found. API documentation cannot be built.")
  set(ENABLE_DEVELDOCS FALSE)
endif ()

