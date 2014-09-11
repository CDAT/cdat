# - FindSciMuparser: Module to find include directories and
#   libraries for Muparser.
#
# Module usage:
#   find_package(SciMuparser ...)
#
# This module will define the following variables:
#  HAVE_MUPARSER, MUPARSER_FOUND = Whether libraries and includes are found
#  Muparser_INCLUDE_DIRS       = Location of Muparser includes
#  Muparser_LIBRARY_DIRS       = Location of Muparser libraries
#  Muparser_LIBRARIES          = Required libraries

######################################################################
#
# FindMuparser: find includes and libraries for muparser
#
# $Id: FindSciMuparser.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################


SciFindPackage(PACKAGE "Muparser"
              INSTALL_DIR "muparser"
              EXECUTABLES ""
              HEADERS "muParser.h"
              LIBRARIES "muparser"
              MODULES "muparser"
              )


if (MUPARSER_FOUND)
  # message(STATUS "Found Muparser")
  set(HAVE_MUPARSER 1 CACHE BOOL "Whether have the MUPARSER library")


else ()
   message(STATUS "Did not find Muparser.  Use -DMUPARSER_DIR to specify the installation directory.")

   if (SciMuparser_FIND_REQUIRED)
       message(FATAL_ERROR "Failing.")
   endif ()

endif ()

