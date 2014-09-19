# - FindSciFftw3: Module to find include directories and
#   libraries for Fftw3
#
# Module usage:
#   find_package(SciFftw3 ...)
#
# This module will define the following variables:
#  HAVE_FFTW3, FFTW3_FOUND = Whether libraries and includes are found
#  Fftw3_INCLUDE_DIRS       = Location of Polyswift includes
#  Fftw3_LIBRARY_DIRS       = Location of Polyswift libraries
#  Fftw3_LIBRARIES          = Required libraries

######################################################################
#
# FindSciFftw3: find includes and libraries for Fftw3
#
# $Id: FindSciFftw3.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (ENABLE_PARALLEL)
  message(STATUS "Looking for parallel FFTW3")
  SciFindPackage(PACKAGE "Fftw3"
                INSTALL_DIR "fftw3-par"
                HEADERS "fftw3.h;rfftw3.h;fftw3_mpi.h;rfftw3_mpi.h"
                LIBRARIES "fftw3;rfftw3;fftw3_mpi;rfftw3_mpi"
                MODULES "fftw3"
                )
else ()
  message(STATUS "Looking for serial FFTW3")
  SciFindPackage(PACKAGE "Fftw3"
                INSTALL_DIR "fftw3"
                HEADERS "fftw3.h;rfftw3.h"
                LIBRARIES "fftw3;rfftw3"
                MODULES "fftw3"
                )
endif ()

if (FFTW3_FOUND)
  message(STATUS "Found Fftw3")
  set(HAVE_FFTW3 1 CACHE BOOL "Whether have the FFTW3library")
else ()
  message(STATUS "Did not find Fftw3.  Use -DFFTW3_DIR to specify the installation directory.")
  if (SciFftw3_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()
