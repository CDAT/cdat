# - FindSciIps: Module to find include directories and
#   libraries for Ips.
#
# Module usage:
#   find_package(SciIps ...)
#
# This module will define the following variables:
#  HAVE_VORPAL, VORPAL_FOUND = Whether libraries and includes are found
#  Ips_INCLUDE_DIRS       = Location of Ips includes
#  Ips_LIBRARY_DIRS       = Location of Ips libraries
#  Ips_LIBRARIES          = Required libraries

#################################################
#
# Find module for Ips installation
#
# $Id: FindSciIps.cmake 1161 2011-12-17 15:44:00Z cary $
#
#################################################

if (DEFINED IPS_DIR)
  message(STATUS "[FindIps.cmake] - IPS_DIR is ${IPS_DIR}")
endif ()

# We have a problem in SciFindPackage
# where if the executable has the same name as a directory
# Find_Program will return the DIRECTORY instead of the executable
# So as a temporary hack-fix, we only look for vorpalser
# JRC 2011-11-18: not seeing this now, so restoring
SciFindPackage(
  PACKAGE Ips
  INSTALL_DIR ips
  EXECUTABLES ips
)

if (IPS_FOUND)

  message(STATUS "Found IPS")
  set(HAVE_IPS 1 CACHE BOOL "Whether have IPS")

# Derive the base Vorpal directory
  set(Ips_FOUND_EXECUTABLE ${Ips_ips})
  if (NOT Ips_FOUND_EXECUTABLE)
    set(Ips_FOUND_EXECUTABLE ${Ips_ips})
  endif ()

# This can only happen if there is something wrong with SciFindPackage,
# and so a fatal error is appropriate.
  if (NOT Ips_FOUND_EXECUTABLE)
    message(FATAL_ERROR "IPS executable was found?  Failing.")
  endif ()

  if (DEBUG_CMAKE)
    message(STATUS "Using path ${Ips_FOUND_EXECUTABLE} to derive base IPS directory.")
  endif ()

  get_filename_component(IPS_DIR ${Ips_FOUND_EXECUTABLE} PATH)
  # get_filename_component(Ips_DIR ${IPS_DIR} PATH)
  get_filename_component(IPS_DIR ${IPS_DIR}/.. REALPATH)
  if (DEBUG_CMAKE)
    message(STATUS "IPS_DIR is ${IPS_DIR}")
  endif ()

  if (DEBUG_CMAKE)
    message("Trying to run executable ${Ips_FOUND_EXECUTABLE} to determine IPS version.")
  endif ()

# IPS_VERSION is required by SciComposerBase.cmake to set the package
# installer name.  It is provided by executing "executable --version",
# and contains version number/revision number.
  include(${CMAKE_SOURCE_DIR}/CMake/SciEngFindVersion.cmake)
  SciEngFindVersion(${Ips_FOUND_EXECUTABLE} EXE_VERSION)
  set(Ips_VERSION ${EXE_VERSION})
  set(IPS_VERSION ${EXE_VERSION})
else ()
  message(STATUS "IPS not found. Use -DIPS_DIR to specify the installation directory.")
  if (SciIps_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
  set(Ips_VERSION "IpsNotFound")
  set(IPS_VERSION "IpsNotFound")
endif ()

