# - FindSciBase: Module to find include directories and
#   libraries for SciBase.
#
# Module usage:
#   find_package(SciBase ...)
#
# This module will define the following variables:
#  HAVE_TXBASE, TXBASE_FOUND = Whether libraries and includes are found
#  SciBase_INCLUDE_DIRS       = Location of SciBase includes
#  SciBase_LIBRARY_DIRS       = Location of SciBase libraries
#  SciBase_LIBRARIES          = Required libraries

######################################################################
#
# FindSciBase: find includes and libraries for txbase
#
# $Id: FindSciBase.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (NOT_HAVE_STD_ABS_DOUBLE)
  set(txbasefindlibs "txbase;txstd")
else ()
  set(txbasefindlibs txbase)
endif ()

if (HAVE_MPI)
  SciFindPackage(PACKAGE "SciBase"
    INSTALL_DIR "txbase-par;txbase-ben"
    HEADERS "txbase_version.h"
    LIBRARIES "${txbasefindlibs}"
    LIBRARY_SUBDIRS "lib/${CXX_COMP_LIB_SUBDIR};lib"
  )
else ()
  SciFindPackage(PACKAGE "SciBase"
    HEADERS "txbase_version.h"
    LIBRARIES "${txbasefindlibs}"
    LIBRARY_SUBDIRS "lib/${CXX_COMP_LIB_SUBDIR};lib"
  )
endif ()

