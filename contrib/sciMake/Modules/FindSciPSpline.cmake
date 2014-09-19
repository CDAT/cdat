# - FindSciPspline: Module to find include directories and libraries
#   for Pspline. This module was implemented as there is no stock
#   CMake module for Pspline.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciPspline REQUIRED)
#
# This module will define the following variables:
#  HAVE_PSPLINE         = Whether have the Pspline library
#  Pspline_INCLUDE_DIRS = Location of Pspline includes
#  Pspline_LIBRARY_DIRS = Location of Pspline libraries
#  Pspline_LIBRARIES    = Required libraries
#  Pspline_STLIBS       = Location of Pspline static library

######################################################################
#
# FindSciPspline: find includes and libraries for pspline
#
# $Id: FindSciPSpline.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

#
# Note that there is a psplineImport that is a dynamic library used for
# python.  That should be in a separate search entirely because one
# should generally not use it for linking (it grabs other libraries and
# appends it in).
#

if (ENABLE_PARALLEL)
  set(instdir pspline-par)
else ()
  set(instdir pspline)
endif ()
set(searchlibs "pspline")

SciFindPackage(PACKAGE "Pspline"
  INSTALL_DIR "${instdir}"
  HEADERS "czspline_capi.h;transp_util.h;pspline_config.h"
  LIBRARIES ${searchlibs}
  LIBRARY_SUBDIRS "lib"
)

set(PSPLINE_DIR ${Pspline_DIR})
if (PSPLINE_FOUND)
  # message(STATUS "Found Pspline")
  set(HAVE_PSPLINE 1 CACHE BOOL "Whether have the Pspline library")
endif ()
