# - FindSciHdf5: Module to find include directories and
#   libraries for Hdf5.
#
# Module usage:
#   find_package(SciHdf5 ...)
#
# This module will define the following variables:
#  HAVE_HDF5, HDF5_FOUND   = Whether libraries and includes are found
#  Hdf5_INCLUDE_DIRS       = Location of Hdf5 includes
#  Hdf5_LIBRARY_DIRS       = Location of Hdf5 libraries
#  Hdf5_LIBRARIES          = Required libraries
#  Hdf5_DLLS               =

######################################################################
#
# FindHdf5: find includes and libraries for hdf5
#
# $Id: FindSciHdf5.cmake 1255 2012-02-09 00:51:27Z cary $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdirs hdf5-par)
else ()
  set(instdirs hdf5)
endif ()

if (USE_SHARED_HDF5 AND WIN32)
  set(desiredlibs hdf5dll)
else ()
  set(desiredlibs hdf5_hl hdf5)
  if (CMAKE_Fortran_COMPILER_WORKS)
    set(desiredlibs hdf5_fortran hdf5_f90cstub ${desiredlibs})
    set(desiredmods hdf5)
  else ()
    set(desiredmods)
  endif ()
endif ()

SciFindPackage(PACKAGE "Hdf5"
  INSTALL_DIR ${instdirs}
  EXECUTABLES h5diff
  HEADERS hdf5.h
  LIBRARIES ${desiredlibs}
  MODULES ${desiredmods}
  INCLUDE_SUBDIRS include include/hdf5/include # Last for VisIt installation
  MODULE_SUBDIRS include/fortran include lib
)

if (HDF5_FOUND)
  # message(STATUS "Found Hdf5")
  set(HAVE_HDF5 1 CACHE BOOL "Whether have the HDF5 library")
  set(OLD_H5S_SELECT_HYPERSLAB_IFC 0 CACHE BOOL
    "Whether using the old 1.6.3 H5Sselect_hyperslab interface")
else ()
  message(STATUS "Did not find Hdf5.  Use -DHDF5_DIR to specify the installation directory.")
  if (SciHdf5_FIND_REQUIRED)
    message(FATAL_ERROR "Failing.")
  endif ()
endif ()

