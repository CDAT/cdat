# - FindSciVorpal: Module to find include directories and
#   libraries for Vorpal.
#
# Module usage:
#   find_package(SciVorpal ...)
#
# This module will define the following variables:
#  HAVE_VORPAL, VORPAL_FOUND = Whether libraries and includes are found
#  Vorpal_INCLUDE_DIRS       = Location of Vorpal includes
#  Vorpal_LIBRARY_DIRS       = Location of Vorpal libraries
#  Vorpal_LIBRARIES          = Required libraries

###########################################################
#
# Find module for Vorpal installation
#
# $Id: FindSciVorpal.cmake 1161 2011-12-17 15:44:00Z cary $
#
###########################################################

if (DEFINED VORPAL_DIR)
  message(STATUS "[FindVorpal.cmake] - VORPAL_DIR is ${VORPAL_DIR}")
endif ()

# We have a problem in SciFindPackage
# where if the executable has the same name as a directory
# Find_Program will return the DIRECTORY instead of the executable
# So as a temporary hack-fix, we only look for vorpalser
# JRC 2011-11-18: not seeing this now, so restoring
SciFindPackage(
  PACKAGE Vorpal
  INSTALL_DIR vorpal
  EXECUTABLES vorpalser vorpal
)

if (VORPAL_FOUND)
  message(STATUS "Found Vorpal")
  set(HAVE_VORPAL 1 CACHE BOOL "Whether have Vorpal")

  # Derive the base VisIt directory
  set(Vorpal_FOUND_EXECUTABLE ${Vorpal_vorpalser})
  if (NOT Vorpal_FOUND_EXECUTABLE)
    set(Vorpal_FOUND_EXECUTABLE ${Vorpal_vorpal})
  endif ()

# This can only happen if there is something wrong with SciFindPackage,
# and so a fatal error is appropriate.
  if (NOT Vorpal_FOUND_EXECUTABLE)
    message(FATAL_ERROR "Neither Vorpal executable was found?  Failing.")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Using path ${Vorpal_FOUND_EXECUTABLE} to derive base Vorpal directory.")
  endif ()

  get_filename_component(VORPAL_DIR ${Vorpal_FOUND_EXECUTABLE} PATH)
  # get_filename_component(VORPAL_DIR ${VORPAL_DIR} PATH)
  get_filename_component(VORPAL_DIR ${VORPAL_DIR}/.. REALPATH)
  if (DEBUG_CMAKE)
    message(STATUS "VORPAL_DIR is ${VORPAL_DIR}")
  endif ()

  if (DEBUG_CMAKE)
    message("Trying to run executable ${Vorpal_FOUND_EXECUTABLE} to determine Vorpal version.")
  endif ()

# Vorpal_VERSION is required by SciComposerBase.cmake to set the package
# installer name.  It is provided by executing "executable --version",
# and contains version number/revision number.
  include(${CMAKE_SOURCE_DIR}/CMake/SciEngFindVersion.cmake)
  SciEngFindVersion(${Vorpal_FOUND_EXECUTABLE} EXE_VERSION)
  set(Vorpal_VERSION ${EXE_VERSION})
  set(VORPAL_VERSION ${EXE_VERSION})
else ()
  message(STATUS "Vorpal not found. Use -DVORPAL_DIR to specify the installation directory.")
  if (SciVorpal_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
  set(Vorpal_VERSION "VorpalNotFound")
  set(VORPAL_VERSION "VorpalNotFound")
endif ()
