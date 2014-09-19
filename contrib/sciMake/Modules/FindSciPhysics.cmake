# - FindSciPhysics: Module to find include directories and
#   libraries for Physics.
#
# Module usage:
#   find_package(SciPhysics ...)
#
# This module will define the following variables:
#  HAVE_TXPHYSICS, TXPHYSICS_FOUND = Whether libraries and includes are found
#  SciPhysics_INCLUDE_DIRS       = Location of Physics includes
#  SciPhysics_LIBRARY_DIRS       = Location of Physics libraries
#  SciPhysics_LIBRARIES          = Required libraries

######################################################################
#
# FindSciPhysics: find includes and libraries for txbase
#
# $Id: FindSciPhysics.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

option(ENABLE_TXPHYSICS "Whether to enable SciPhysics" ON)
if (WIN32)
  set(txphysics_statlib txphysics.lib)
else ()
  set(txphysics_statlib libtxphysics.a)
endif ()

if (ENABLE_TXPHYSICS)
  if (HAVE_MPI)
    SciFindPackage(PACKAGE "SciPhysics"
      INSTALL_DIRS "txphysics-ben;txphysics"
      HEADERS "txphysics_version.h"
      LIBRARIES "${txphysics_statlib}"
    )
  else ()
    SciFindPackage(PACKAGE "SciPhysics"
      HEADERS "txphysics_version.h"
      LIBRARIES "${txphysics_statlib}"
    )
  endif ()
endif ()

if (TXPHYSICS_FOUND)
  # message(STATUS "Found SciPhysics.")
  set(HAVE_TXPHYSICS 1 CACHE BOOL "Whether have the SciPhysics library")
else ()
  message(WARNING "Did not find SciPhysics.  Use -DTXPHYSICS_DIR to specify the installation directory.")
  if (SciPhysics_FIND_REQUIRED)
    message(FATAL_ERROR "Failing.")
  endif ()
endif ()

