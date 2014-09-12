# - FindSciSquish: Module to find executables and libraries
#   for Squish. A stock CMake FindSquish.cmake does exist but
#   is supported only for Squish version 3.  
#
# This module can be included in CMake builds in find_package:
#   find_package(SciSquish REQUIRED)
#
# This module will define the following variables:
#  HAVE_SQUISH         = Whether have the Squish library
#  Squish_EXECUTABLES  = Location of Squish executables
#  Squish_LIBRARY_DIRS = Location of Squish libraries
#  Squish_LIBRARIES    = Required libraries

######################################################################
#
# SciFindSquish: find includes and libraries for Squish.
#
# $Id: FindSciSquish.cmake 1254 2012-02-08 22:54:27Z jdelamere $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH};/contrib)

SciFindPackage(PACKAGE "Squish"
              INSTALL_DIR "squish"
              EXECUTABLES "squishrunner;squishserver"
              LIBRARIES "squishqtpre"
)

if (SQUISH_FOUND)
  message(STATUS "Found Squish")
  set(HAVE_SQUISH 1 CACHE BOOL "Whether have the SQUISH package")
else ()
  message(STATUS "Did not find Squish.  Use -DSQUISH_DIR to specify the installation directory.")
endif ()

