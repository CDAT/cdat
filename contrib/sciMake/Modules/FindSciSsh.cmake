# - FindSciSsh: Module to find include directories and libraries for
#   SciSsh. SciSsh is an in-house C++ Ssh package that uses ne7ssh api.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciSsh REQUIRED)
#
# This module will define the following variables:
#  HAVE_TXSSH         = Whether have the SciSsh library
#  SciSsh_INCLUDE_DIRS = Location of SciSsh includes
#  SciSsh_LIBRARY_DIRS = Location of SciSsh libraries
#  SciSsh_LIBRARIES    = Required libraries, libSciSsh

######################################################################
#
# FindSciSsh: find includes and libraries for SciSsh.
#
# $Id: FindSciSsh.cmake 1151 2011-12-17 13:51:42Z cary $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH};/contrib)

SciFindPackage(PACKAGE "SciSsh"
              INSTALL_DIR "txssh"
              HEADERS "SciSsh.h"
              LIBRARIES "txssh"
              )

if (TXSSH_FOUND)
  message(STATUS "Found SciSsh")
  set(HAVE_TXSSH 1 CACHE BOOL "Whether have the SciSsh library")
else ()
  message(STATUS "Did not find SciSsh.  Use -DTXSSH_DIR to specify the installation directory.")
  if (SciSsh_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

