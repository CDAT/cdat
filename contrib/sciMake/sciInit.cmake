######################################################################
#
# sciInit: Do the startup stuff for any package
#
# $Id: sciInit.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

#####################################################################
#
# Pull in useful macros
#
#####################################################################

if (NOT DEFINED TXCMAKE_DIR)
  set(TXCMAKE_DIR ${PROJECT_SOURCE_DIR}/sciMake)
endif ()
include(${TXCMAKE_DIR}/sciFuncsMacros.cmake)
include(${TXCMAKE_DIR}/sciGetDepsFromInstall.cmake)

#####################################################################
#
# Clean out config.summary
#
#####################################################################

set(CONFIG_SUMMARY ${PROJECT_BINARY_DIR}/config.summary)
file(REMOVE ${CONFIG_SUMMARY})
sciPrintString("CONFIGURING ${CMAKE_PROJECT_NAME} with CMake in "
 "${PROJECT_BINARY_DIR}.")

#####################################################################
#
# Set some vars to upper case for case-free comparisons
#
#####################################################################

string(TOUPPER "${CMAKE_BUILD_TYPE}" CMAKE_BUILD_TYPE_UC)

#####################################################################
#
# Set group write installation perm
# May need to be different on different machines
#
#####################################################################

if (NOT DEFINED TX_GROUP_WRITE)
  set(TX_GROUP_WRITE ""
      CACHE STRING "Value of group write permissions")
endif ()
if (NOT DEFINED TX_WORLD_PERMS)
  set(TX_WORLD_PERMS ""
      CACHE STRING "Value of world permissions")
endif ()

#####################################################################
#
# Set OS-specific flags
#
#####################################################################

message(STATUS "[sciInit.cmake] CMAKE_SYSTEM_NAME is ${CMAKE_SYSTEM_NAME}")
if ("${CMAKE_SYSTEM_NAME}" MATCHES "Darwin")
  set(MACX TRUE CACHE BOOL "True if compiled on Mac OS X")
  message(STATUS "[sciInit.cmake] Compiling on MAC")
elseif ("${CMAKE_SYSTEM_NAME}" MATCHES "Linux")
  set(LINUX TRUE CACHE BOOL "True if compiled on Linux")
  message(STATUS "[sciInit.cmake] Compiling on LINUX")
elseif (WIN32)
  set(WINDOWS TRUE CACHE BOOL "True if compiled on Windows")
  message(STATUS "[sciInit.cmake] Compiling on WINDOWS")
else ()
  message(FATAL_ERROR "[sciInit.cmake] Unrecognized OS!")
endif ()

######################################################################
#
# Set up standard paths
#
######################################################################

# message(STATUS "CMAKE_MODULE_PATH = ${CMAKE_MODULE_PATH}.")
# message(STATUS "CMAKE_ROOT = ${CMAKE_ROOT}.")
# if (NOT DEFINED CMAKE_MODULE_PATH)
#  set(CMAKE_MODULE_PATH ${CMAKE_ROOT}/Modules)
# endif ()
# message(STATUS "CMAKE_MODULE_PATH = ${CMAKE_MODULE_PATH}.")
cmake_policy(SET CMP0017 OLD) # Use our modules over theirs
set(CMAKE_MODULE_PATH
  ${TXCMAKE_DIR}/Modules
)
if (DEBUG_CMAKE)
  message(STATUS "CMAKE_MODULE_PATH = ${CMAKE_MODULE_PATH}")
endif ()

# message("SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH}")
if (SUPRA_SEARCH_PATH)
  set(SUPRA_SEARCH_PATH "${SUPRA_SEARCH_PATH}")
else ()
  if (WIN32)
# According to JRC the following must be turned off to compile under
# Windows(AP)
    # set(SUPRA_SEARCH_PATH $ENV{HOME}/software /winsame/internal /winsame/volatile /winsame/contrib /opt /usr/local)
  else ()
# JRC: SUPRA_SEARCH_PATH should include only top directory
# also, system paths should not be needed due to cmake's system search
    set(SUPRA_SEARCH_PATH $ENV{HOME}/software /internal /volatile /contrib /opt /usr/local)
  endif ()
endif ()
sciPrintString("SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH}")

######################################################################
#
# Get system description
#
######################################################################

find_program(HOSTNAME_CMD NAMES hostname)
exec_program(${HOSTNAME_CMD} ARGS OUTPUT_VARIABLE HOSTNAME)
sciPrintString("CMake running on ${HOSTNAME}")
string(REGEX REPLACE "\\..*$" "" UQHOSTNAME "${HOSTNAME}")
sciPrintString("UQHOSTNAME = ${UQHOSTNAME}")

find_program(UNAME NAMES uname)
macro(getuname name flag)
  exec_program("${UNAME}" ARGS "${flag}" OUTPUT_VARIABLE "${name}")
endmacro(getuname)

getuname(osname -s)
getuname(osrel  -r)
getuname(cpu    -m)
set(HOSTTYPE "${osname}-${cpu}")
sciPrintString("HOSTTYPE = ${HOSTTYPE}")
site_name(HOSTNAME)
sciPrintString("hostname is ${HOSTNAME}")

######################################################################
#
# Other useful provenance
#
######################################################################

# Calling it "ENV Var" + "NAME" to avoid conflicts elsewhere
set(HOMENAME $ENV{HOME})
set(USERNAME $ENV{USER})
set(SCRATCHNAME $ENV{SCRATCH})

######################################################################
#
# Get revisions
#
######################################################################

include(${TXCMAKE_DIR}/sciSvnInfo.cmake)

######################################################################
#
# config.h
#
######################################################################

if (NOT NO_CONFIG_H)
  if (WIN32)
    add_definitions(/DHAVE_CONFIG_H)
  else ()
    add_definitions(-DHAVE_CONFIG_H)
  endif ()
endif ()

######################################################################
#
# Fix shared flags on windows
#
######################################################################

include(${TXCMAKE_DIR}/sciWinFlags.cmake)

######################################################################
#
# C, CXX, Fortran Checks
#
######################################################################

include(${TXCMAKE_DIR}/sciCChecks.cmake)
if (NOT NOCXX)
  include(${TXCMAKE_DIR}/sciCxxChecks.cmake)
endif ()
if (NOT NOFORTRAN)
  message("")
  message("--------- sciFortranChecking ---------")
# Enable Fortran to all those variables
  enable_language(Fortran)
  include(${CMAKE_ROOT}/Modules/CMakeDetermineFortranCompiler.cmake)
  include(${TXCMAKE_DIR}/sciFortranChecks.cmake)
else ()
  message(STATUS "No Fortran, so no implicit fortran link libraries known.")
endif ()

######################################################################
#
# Load Find Package
#
######################################################################

include(${TXCMAKE_DIR}/Modules/SciFindPackage.cmake)

######################################################################
#
# Look for MPI
#
######################################################################

option(ENABLE_PARALLEL "Enable parallel build" OFF)
if (ENABLE_PARALLEL)
  message("")
  message(STATUS "ENABLE_PARALLEL requested.  Will search for MPI.")
elseif (INSTALL_PARALLEL)
  message("")
  message(STATUS "INSTALL_PARALLEL requested.  Will search for MPI.")
else ()
  message(STATUS "Not searching for MPI.")
endif ()
if (ENABLE_PARALLEL OR INSTALL_PARALLEL)
  find_package(sciMpi REQUIRED)
endif ()

######################################################################
#
# Host information
#
######################################################################


######################################################################
#
# Mac OS X make dylibs install with rpath
#
######################################################################

if (APPLE)
  set (CMAKE_INSTALL_NAME_DIR ${CMAKE_INSTALL_PREFIX})
endif ()

