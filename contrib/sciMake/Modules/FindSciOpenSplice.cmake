# - FindSciOpenSplice: Module to find include directories and libraries
#   for OpenSplice. This module was implemented as there is no stock
#   CMake module for OpenSplice. This is currently being used by QuIDS
#   project.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciOpenSplice REQUIRED)
#
# This module will define the following variables:
#  HAVE_OPENSPLICE             = Whether have the Cfitsio library
#  OpenSplice_EXECUTABLES      = Location of OpenSplice's idlpp executable
#  OpenSplice_INCLUDE_DIRS     = Location of OpenSplice include dir
#  OpenSplice_LIBRARY_DIRS     = Location of OpenSplice libraries
#  OpenSplice_LIBRARIES        = List of all the necessary libraries
#  OpenSplice_CPP_INCLUDE_DIRS = List of OpenSplice's C++ include directories
#                                           to be used by C++ applications
#  OpenSplice_C_INCLUDE_DIRS   = List of OpenSplice's C include directories
#                                           to be used by C applications
#  OpenSplice_idlpp            = Location to OpenSplice's idlpp executable

######################################################################
#
# FindOpenSplice: find includes and libraries for OpenSplice. Complex
# due to the many libraries that OpenSplice has
#
# $Id: FindSciOpenSplice.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################
set(SUPRA_SEARCH_PATH ${SUPRA_SEARCH_PATH};/contrib)

SciFindPackage(PACKAGE "OpenSplice"
              INSTALL_DIR "opensplice"
              EXECUTABLES "idlpp"
              HEADERS "dcps"
              LIBRARIES "ddsdatabase;dcpsgapi;dcpssacpp;dcpssac;ddsos;ddsutil;ddsserialization;ddskernel;ddsuser;ddsosnet;ddsconf;ddsconfparser"
              )

if (OPENSPLICE_FOUND)
  message(STATUS "Found OpenSplice")
  # Get the root OSPL installation location
  get_filename_component(OpenSplice_DIR ${OpenSplice_INCLUDE_DIRS} PATH)

  set(OpenSplice_CPP_INCLUDE_DIRS
          ${OpenSplice_INCLUDE_DIRS}/dcps/C++/SACPP
          ${OpenSplice_INCLUDE_DIRS}
          ${OpenSplice_INCLUDE_DIRS}/sys
  )
  sciPrintString("  OpenSplice_CPP_INCLUDE_DIRS     = ${OpenSplice_CPP_INCLUDE_DIRS}")

  set(OpenSplice_C_INCLUDE_DIRS
          ${OpenSplice_INCLUDE_DIRS}/dcps/C/SAC
          ${OpenSplice_INCLUDE_DIRS}
          ${OpenSplice_INCLUDE_DIRS}/sys
  )
  sciPrintString("  OpenSplice_C_INCLUDE_DIRS       = ${OpenSplice_C_INCLUDE_DIRS}")

  if (WIN32)
    set(OpenSplice_release_com ${OpenSplice_DIR}/release.bat)
    set(OpenSplice_EXEC ${OpenSplice_release_com})
  else ()
    set(OpenSplice_release_com ${OpenSplice_DIR}/release.com)
    set(OpenSplice_EXEC "/bin/sh" ${OpenSplice_release_com})
  endif ()

  set(HAVE_OPENSPLICE 1 CACHE BOOL "Whether have the OPENSPLICE library")
else ()
  message(STATUS "Did not find OpenSplice.  Use -DOPENSPLICE_DIR to specify the installation directory.")
  if (SciOpenSplice_FIND_REQUIRED)
    message(FATAL_ERROR "Failing.")
  endif ()
endif ()

