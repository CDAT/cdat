# - FindSciFacets: Module to find include directories and
#   libraries for Facets.
#
# Module usage:
#   find_package(SciFacets ...)
#
# This module will define the following variables:
#  HAVE_FACETS, FACETS_FOUND = Whether libraries and includes are found
#  Facets_INCLUDE_DIRS       = Location of Facets includes
#  Facets_LIBRARY_DIRS       = Location of Facets libraries
#  Facets_LIBRARIES          = Required libraries
#  Facets_FOUND_EXECUTABLE
#  Facets_VERSION
#  FACETS_VERSION

######################################################################
#
# Find module for Facets installation
#
# $Id: FindSciFacets.cmake 1161 2011-12-17 15:44:00Z cary $
#
######################################################################

if (DEFINED FACETS_DIR)
  message(STATUS "[FindFacets.cmake] - FACETS_DIR is ${FACETS_DIR}")
endif ()

# We have a problem in SciFindPackage
# where if the executable has the same name as a directory
# Find_Program will return the DIRECTORY instead of the executable
# So as a temporary hack-fix, we only look for facetsser

SciFindPackage(
        PACKAGE Facets
        INSTALL_DIR facets
        EXECUTABLES facetserst facetsst
        )

if (FACETS_FOUND)
  message(STATUS "Found Facets")
  set(HAVE_FACETS 1 CACHE BOOL "Whether have Facets")

  # Derive the base VisIt directory
  set(Facets_FOUND_EXECUTABLE ${Facets_facetserst})
  if (NOT Facets_FOUND_EXECUTABLE)
    set(Facets_FOUND_EXECUTABLE ${Facets_facetsst})
  endif ()

  if (NOT Facets_FOUND_EXECUTABLE)
    message(FATAL_ERROR "Neither Facets executable was found?  Failing.")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Using path ${Facets_FOUND_EXECUTABLE} to derive base Facets directory.")
  endif ()

  get_filename_component(FACETS_DIR
    ${Facets_FOUND_EXECUTABLE}/../..
    REALPATH
  )
  # if(DEBUG_CMAKE)
    message(STATUS "FACETS_DIR is ${FACETS_DIR}")
  # endif()

# Facets_VERSION is required by        SciComposerBase.cmake to        set the        package        installer name.
# It is        provided by executing "executable --version", and contains version number/revision number.
  include(${CMAKE_SOURCE_DIR}/CMake/SciEngFindVersion.cmake)
  SciEngFindVersion(${Facets_FOUND_EXECUTABLE} EXE_VERSION)
  set(Facets_VERSION ${EXE_VERSION})
  set(FACETS_VERSION ${EXE_VERSION})
else ()
  message(STATUS "Facets not found. Use -DFACETS_DIR to specify the installation directory.")
  if (SciFacets_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
  set(Facets_VERSION "FacetsNotFound")
  set(FACETS_VERSION "FacetsNotFound")
endif ()

