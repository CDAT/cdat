# - FindSciBoost: Module to find include directories and libraries for
#   Boost. This module was originally developed to set the
#   SUPRA_SEARCH_PATH, so that the system path was not checked before
#   the user specified path, and included the stock FindBoost. This
#   changed after quite a few modifications, as it would still look at
#   system path if the libraries weren't found in teh user specified
#   path.
#   Will be modified soon to include the stock FindBoost
#
# This module can be included in CMake builds using find_package:
#   find_package(SciBoost REQUIRED signals filesystem system ...)
#
# The components list needs to contain actual names of boost libraries
# only: signals for libboost_signals, system for libboost_system, etc.
#
# This module will define the following variables:
#   BOOST_FOUND, HAVE_BOOST = True if Boost is found: the include directory was
#                             found and all the libraries specified were found.
#   Boost_INCLUDE_DIRS      = Location of Boost includes
#   Boost_LIBRARY_DIRS      = Location of Boost libraries
#   Boost_LIBRARIES         = Required libraries

######################################################################
#
# FindSciBoost: find includes and libraries for boost
#
# $Id: FindSciBoost.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

#
# Append boost_ to all the component names
#
set(SciBoost_LIBRARY_LIST "")
foreach (COMPONENT ${SciBoost_FIND_COMPONENTS})
  set(SciBoost_LIBRARY_LIST ${SciBoost_LIBRARY_LIST} boost_${COMPONENT})
endforeach ()

SciFindPackage(PACKAGE "Boost"
              INSTALL_DIR "boost"
              HEADERS "boost/mpi.hpp"
              LIBRARIES "${SciBoost_LIBRARY_LIST}"
              )
unset(SciBoost_LIBRARY_LIST CACHE)

if (BOOST_FOUND AND NOT Boost_INCLUDE_DIRS)
  set(BOOST_FOUND FALSE)
  message(STATUS "Reversing Boost found as Boost_INCLUDE_DIRS is empty.")
endif ()

if (BOOST_FOUND)
  # message(STATUS "Found Boost")
  set(HAVE_BOOST 1 CACHE BOOL "Whether have the Boost library")
else ()
  message(STATUS "Did not find Boost.  Use -DBOOST_DIR to specify installation directory.")
  if (SciBoost_FIND_REQUIRED)
    message(FATAL_ERROR "Failed finding boost.")
  endif ()
endif ()

