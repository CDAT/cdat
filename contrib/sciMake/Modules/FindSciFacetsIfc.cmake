# - FindSciFacetsIfc: Module to find include directories and
#   libraries for FacetsIfc.
#
# Module usage:
#   find_package(SciFacetsIfc ...)
#
# This module will define the following variables:
#  HAVE_FACETSIFC, FACETSIFC_FOUND = Whether libraries and includes are found
#  FacetsIfc_INCLUDE_DIRS       = Location of FacetsIfc includes
#  FacetsIfc_LIBRARY_DIRS       = Location of FacetsIfc libraries
#  FacetsIfc_LIBRARIES          = Required libraries

######################################################################
#
# FindSciGeom: find includes for FacetsIfc
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

SciFindPackage(PACKAGE "FacetsIfc"
              HEADERS "FacetsIfc.h"
              )

if (FACETSIFC_FOUND)
  message(STATUS "Found FacetsIfc")
  set(HAVE_FACETSIFC 1 CACHE BOOL "Whether have FacetsIfc")
else ()
  message(STATUS "Did not find FacetsIfc.  Use _DFACETSIFC_DIR to specify the installation directory.")
  if (SciFacetsIfc_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()
