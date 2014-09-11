# - FindSciSimD: Module to find include directories and libraries for
#   SimD. This module was implemented as there is no stock CMake
#   module for SimD. This is currently being used by QuIDS project.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciSimD REQUIRED)
#
# This module will define the following variables:
#  HAVE_SIMD         = Whether have the SimD library
#  SimD_INCLUDE_DIRS = Location of SimD includes
#  SimD_LIBRARY_DIRS = Location of SimD libraries
#  SimD_LIBRARIES    = Required libraries, libSimD

######################################################################
#
# FindSciSimD: find includes and libraries for Simd.
#
# $Id: FindSciSimD.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH};/contrib)

SciFindPackage(PACKAGE "SimD"
              INSTALL_DIR "simd"
              HEADERS "dds"
              LIBRARIES "SimD"
              )

if (SIMD_FOUND)
  message(STATUS "Found SimD")
  set(HAVE_SIMD 1 CACHE BOOL "Whether have the SIMD library")
else ()
  message(STATUS "Did not find SimD.  Use -DSIMD_DIR to specify the installation directory.")
  if (SciSimD_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

