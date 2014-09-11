# - FindSciThrust: Module to find include directories and
#   libraries for Thrust.
#
# Module usage:
#   find_package(SciThrust ...)
#
# This module will define the following variables:
#  HAVE_THRUST, THRUST_FOUND = Whether libraries and includes are found
#  Thrust_INCLUDE_DIRS       = Location of Thrust includes
#  Thrust_LIBRARY_DIRS       = Location of Thrust libraries
#  Thrust_LIBRARIES          = Required libraries

######################################################################
#
# FindSciThrust: find includes and libraries for thrust
#
# $Id: FindSciThrust.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

SciFindPackage(PACKAGE "Thrust"
              INSTALL_DIR "thrust"
              INSTALL_DIRS "."
#              EXECUTABLES "."
              HEADERS "device_vector.h"
#              LIBRARIES "."
#              MODULES "."
              EXECUTABLE_SUBDIRS "."
              INCLUDE_SUBDIRS "."
              LIBRARY_SUBDIRS "."
              )

if (THRUST_FOUND)
  message("Found Thrust.")
  set(HAVE_THRUST 1 CACHE BOOL "Whether have the Thrust library.")
else ()
  if (SciThrust_FIND_REQUIRED)
    message(FATAL_ERROR "Thrust not found.  Install it or perhaps add -DTHRUST_DIR=<dir>")
  else ()
    message("Did not find Thrust. Thrust is located in CUDA 4.0+, but needed for CUDA 3.2.")
    message("If using CUDA 3.2, consider adding -DTHRUST_DIR=<dir>")
  endif ()
endif ()

