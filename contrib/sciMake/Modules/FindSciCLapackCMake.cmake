# - FindSciCLapackCMake: Module to find include directories and
#   libraries for CLapackCMake.
#
# Module usage:
#   find_package(SciCLapackCMake ...)
#
# This module will define the following variables:
#  HAVE_CLAPACKCMAKE, CLAPACKCMAKE_FOUND = Whether libraries and includes are found
#  CLapackCMake_INCLUDE_DIRS = Location of CLapackCMake includes
#  CLapackCMake_LIBRARY_DIRS = Location of CLapackCMake libraries
#  CLapackCMake_LIBRARIES    = Required libraries

######################################################################
#
# FindCLapackCMake: find includes and libraries for txbase
#
# $Id: FindSciCLapackCMake.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

SciFindPackage(PACKAGE "CLapackCMake"
              INSTALL_DIR "clapack_cmake"
              HEADERS "clapack.h;f2c.h;blaswrap.h"
              LIBRARIES "lapack;blas;f2c"
              )

if (CLAPACKCMAKE_FOUND)
  message(STATUS "CLapackCMake found.")
  set(HAVE_CLAPACKCMAKE 1 CACHE BOOL "Whether have CLapackCMake")
else ()
  message(STATUS "Did not find CLapackCMake.  Use -DCLAPACKCMAKE_DIR to specify the installation directory.")
  if (SciCLapackCMake_FIND_REQUIRED)
    message(FATAL_ERROR "Failed")
  endif ()
endif ()
