# - FindSciGeom: Module to find include directories and
#   libraries for Geom.
#
# Module usage:
#   find_package(SciGeom ...)
#
# This module will define the following variables:
#  HAVE_TXGEOM, TXGEOM_FOUND = Whether libraries and includes are found
#  SciGeom_INCLUDE_DIRS       = Location of Geom includes
#  SciGeom_LIBRARY_DIRS       = Location of Geom libraries
#  SciGeom_LIBRARIES          = Required libraries


######################################################################
#
# FindSciGeom: find includes and libraries for txgeom
#
# $Id: FindSciGeom.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (HAVE_MPI)
  SciFindPackage(PACKAGE "SciGeom"
    INSTALL_DIR "txgeom-par;txgeom-ben"
    HEADERS "txgeom_version.h"
    LIBRARIES "txgeomtransforms;txgeomlibrary;txgeommesh;txgeomgeometry;txgeomtopology;txgeombase"
    LIBRARY_SUBDIRS "lib/${CXX_COMP_LIB_SUBDIR};lib"
  )
else ()
  SciFindPackage(PACKAGE "SciGeom"
    HEADERS "txgeom_version.h"
    LIBRARIES "txgeomtransforms;txgeomlibrary;txgeommesh;txgeomgeometry;txgeomtopology;txgeombase"
    LIBRARY_SUBDIRS "lib/${CXX_COMP_LIB_SUBDIR};lib"
  )
endif ()

if (TXGEOM_FOUND)
  # message(STATUS "Found SciGeom.")
  set(HAVE_TXGEOM 1 CACHE BOOL "Whether have SciGeom library")
else ()
  message(STATUS "Did not find SciGeom.  Use -DTXGEOM_DIR to specify the installation directory.")
  if (SciGeom_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

