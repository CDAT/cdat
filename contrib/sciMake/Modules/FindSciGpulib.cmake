# - FindSciGpulib: Module to find include directories and
#   libraries for Gpulib.
#
# Module usage:
#   find_package(SciGpulib ...)
#
# This module will define the following variables:
#  HAVE_GPULIB, GPULIB_FOUND = Whether libraries and includes are found
#  Gpulib_INCLUDE_DIRS       = Location of Gpulib includes
#  Gpulib_LIBRARY_DIRS       = Location of Gpulib libraries
#  Gpulib_LIBRARIES          = Required libraries

######################################################################
#
# FindGpulib.cmake: Find the GPU libraries
#
# $Id: FindSciGpulib.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

SciFindPackage(PACKAGE "Gpulib"
              INSTALL_DIR ""   # Empty?  Seems like we want a value here
              HEADERS "gpuPhysicsOp.h"
              LIBRARIES "gpulib"
              LIBRARY_SUBDIRS "lib/${CXX_COMP_LIB_SUBDIR};lib"
              )

if (GPULIB_FOUND)
  message(STATUS "Found gpulib")
  set(HAVE_GPULIB 1 CACHE BOOL "Whether have gpulib")
else ()
  message(STATUS "Did not find gpulib.  Use -DGPULIB_DIR to specify the installation directory.")
  if (SciGpulib_FIND_REQUIRED)
    message(FATAL_ERROR "Failed")
  endif ()
endif ()
