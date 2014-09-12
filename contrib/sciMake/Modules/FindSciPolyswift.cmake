# - FindSciPolyswift: Module to find include directories and
#   libraries for Polyswift.
#
# Module usage:
#   find_package(SciPolyswift ...)
#
# This module will define the following variables:
#  HAVE_POLYSWIFT, POLYSWIFT_FOUND = Whether libraries and includes are found
#  Polyswift_INCLUDE_DIRS       = Location of Polyswift includes
#  Polyswift_LIBRARY_DIRS       = Location of Polyswift libraries
#  Polyswift_LIBRARIES          = Required libraries

#################################################
#
# Find module for Polyswift installation
#
# $Id: FindSciPolyswift.cmake 1161 2011-12-17 15:44:00Z cary $
#
#################################################

if (DEFINED POLYSWIFT_DIR)
  message(STATUS "[FindSciPolyswift.cmake] - POLYSWIFT_DIR is ${POLYSWIFT_DIR}")
endif ()

SciFindPackage(
        PACKAGE Polyswift
        INSTALL_DIR polyswift
        EXECUTABLES "polyswift;polyswiftser"
        )

if (POLYSWIFT_FOUND)
  message(STATUS "Found Polyswift")
  set(HAVE_POLYSWIFT 1 CACHE BOOL "Whether have Polyswift")

  # Derive the base VisIt directory
  set(Polyswift_FOUND_EXECUTABLE ${Polyswift_polyswift})
  if (NOT Polyswift_FOUND_EXECUTABLE)
    set(Polyswift_FOUND_EXECUTABLE ${Polyswift_polyswiftser})
  endif ()

  if (NOT Polyswift_FOUND_EXECUTABLE)
    message(FATAL_ERROR "Neither Polyswift executable was found?  Failing.")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Using path ${Polyswift_FOUND_EXECUTABLE} to derive base Polyswift directory.")
  endif ()

  get_filename_component(POLYSWIFT_DIR
        ${Polyswift_FOUND_EXECUTABLE}
        PATH
        )
  get_filename_component(POLYSWIFT_DIR
        ${POLYSWIFT_DIR}
        PATH
        )
  get_filename_component(POLYSWIFT_DIR
        ${POLYSWIFT_DIR}
        REALPATH
        )
  if (DEBUG_CMAKE)
    message(STATUS "POLYSWIFT_DIR is ${POLYSWIFT_DIR}")
  endif ()

  if (DEBUG_CMAKE)
    message("Trying to run executable ${Polyswift_FOUND_EXECUTABLE} to determine Polyswift version.")
  endif ()
# Polyswift_VERSION is required by SciComposerBase.cmake to set the package installer name.
# It is provided by executing "executable --version", and contains version number/revision number.
  include(${CMAKE_SOURCE_DIR}/CMake/SciEngFindVersion.cmake)
  SciEngFindVersion(${Polyswift_FOUND_EXECUTABLE} EXE_VERSION)
  set(Polyswift_VERSION ${EXE_VERSION})
  set(POLYSWIFT_VERSION ${EXE_VERSION})
else ()
  message(STATUS "Polyswift not found. Use -DPOLYSWIFT_DIR to specify the installation directory.")
  if (SciPolyswift_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
  set(Polyswift_VERSION "PolyswiftNotFound")
  set(POLYSWIFT_VERSION "PolyswiftNotFound")
endif ()
