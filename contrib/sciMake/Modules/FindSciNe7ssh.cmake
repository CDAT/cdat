# - FindSciNe7sshn: Module to find include directories and libraries
#   for Ne7ssh. This module was implemented as there is no stock
#   CMake module for Ne7ssh. This is currently being used by QuIDS
#   project.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciNe7ssh REQUIRED)
#
# This module will define the following variables:
#  HAVE_NE7SSH         = Whether have the Ne7ssh library
#  Ne7ssh_INCLUDE_DIRS = Location of Ne7ssh includes
#  Ne7ssh_LIBRARY_DIRS = Location of Ne7ssh libraries
#  Ne7ssh_LIBRARIES    = Required libraries
#  Ne7ssh_STLIBS       = Location of Ne7ssh static library

######################################################################
#
# SciFindNe7ssh: find includes and libraries for Ne7ssh.
#
# $Id: FindSciNe7ssh.cmake 1151 2011-12-17 13:51:42Z cary $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH};/contrib)

SciFindPackage(PACKAGE "Ne7ssh"
              INSTALL_DIR "ne7ssh"
              HEADERS "ne7ssh.h"
              LIBRARIES "net7ssh"
              )

if (NE7SSH_FOUND)
  message(STATUS "Found Ne7ssh")
  set(HAVE_NE7SSH 1 CACHE BOOL "Whether have the NE7SSH library")
else ()
  message(STATUS "Did not find Ne7ssh.  Use -DNE7SSH_DIR to specify the installation directory.")
  if (SciNe7ssh_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

