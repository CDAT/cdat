# - FindSciNautilus: Module to find include directories and
#   libraries for Nautilus.
#
# Module usage:
#   find_package(SciNautilus ...)
#
# This module will define the following variables:
#  HAVE_NAUTILUS, NAUTILUS_FOUND = Whether libraries and includes are found
#  Nautilus_INCLUDE_DIRS       = Location of Nautilus includes
#  Nautilus_LIBRARY_DIRS       = Location of Nautilus libraries
#  Nautilus_LIBRARIES          = Required libraries

#################################################
#
# Find module for Nautilus installation
#
# $Id: FindSciNautilus.cmake 1161 2011-12-17 15:44:00Z cary $
#
#################################################

if (DEFINED NAUTILUS_DIR)
  message(STATUS "[FindSciNautilus.cmake] - NAUTILUS_DIR is ${NAUTILUS_DIR}")
endif ()

SciFindPackage(
        PACKAGE Nautilus
        INSTALL_DIR nautilus
        EXECUTABLES nautilusser nautilus
        )

if (NAUTILUS_FOUND)
  message(STATUS "Found Nautilus")
  set(HAVE_NAUTILUS 1 CACHE BOOL "Whether have Nautilus")

  # Derive the base VisIt directory
  set(Nautilus_FOUND_EXECUTABLE ${Nautilus_nautilusser})
  if (NOT Nautilus_FOUND_EXECUTABLE)
    set(Nautilus_FOUND_EXECUTABLE ${Nautilus_nautilus})
  endif ()

  if (NOT Nautilus_FOUND_EXECUTABLE)
    message(FATAL_ERROR "Neither Nautilus executable was found?  Failing.")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Using path ${Nautilus_FOUND_EXECUTABLE} to derive base Nautilus directory.")
  endif ()

  get_filename_component(NAUTILUS_DIR
        ${Nautilus_FOUND_EXECUTABLE}
        PATH
        )
  get_filename_component(NAUTILUS_DIR
        ${NAUTILUS_DIR}
        PATH
        )
  get_filename_component(NAUTILUS_DIR
        ${NAUTILUS_DIR}
        REALPATH
        )
  if (DEBUG_CMAKE)
    message(STATUS "NAUTILUS_DIR is ${NAUTILUS_DIR}")
  endif ()
# Nautilus_VERSION is required by SciComposerBase.cmake to set the package installer name.
# It is provided by executing "executable --version", and contains version number/revision number.
  include(${CMAKE_SOURCE_DIR}/CMake/SciEngFindVersion.cmake)
  SciEngFindVersion(${Nautilus_FOUND_EXECUTABLE} EXE_VERSION)
  set(Nautilus_VERSION ${EXE_VERSION})
  set(NAUTILUS_VERSION ${EXE_VERSION})
else ()
  message(STATUS "Nautilus not found. Use -DNAUTILUS_DIR to specify the installation directory.")
  if (SciNautilus_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
  set(Nautilus_VERSION "NautilusNotFound")
  set(NAUTILUS_VERSION "NautilusNotFound")
endif ()
