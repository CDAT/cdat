# - FindSciMesa: Module to find include directories and
#   libraries for Mesa.
#
# Module usage:
#   find_package(SciMesa ...)
#
# This module will define the following variables:
#  HAVE_MESA, MESA_FOUND = Whether libraries and includes are found
#  Mesa_INCLUDE_DIRS       = Location of Mesa includes
#  Mesa_LIBRARY_DIRS       = Location of Mesa libraries
#  Mesa_LIBRARIES          = Required libraries

#################################################
#
# Find module for Mesa
#
# $Id: FindSciMesa.cmake 1161 2011-12-17 15:44:00Z cary $
#
#################################################

if (NOT DEFINED Mesa_LIBRARY_LIST)
  set(Mesa_LIBRARY_LIST MesaGL OSMesa)
endif ()

# JRC: I believe it is conventional to look for includes with the GL dir
# prepended.
SciFindPackage(
  PACKAGE Mesa
  HEADERS GL/gl.h
  LIBRARIES MesaGL OSMesa
  INCLUDE_SUBDIRS include include/mesa/include # First for Visit
)

if (MESA_FOUND)
  # message(STATUS "[FindMesa.cmake] - Found Mesa")
  # message(STATUS "[FindMesa.cmake] - Mesa_INCLUDE_DIRS = ${Mesa_INCLUDE_DIRS}")
  # message(STATUS "[FindMesa.cmake] - Mesa_LIBRARIES = ${Mesa_LIBRARIES}")
  set(HAVE_Mesa 1 CACHE BOOL "Whether have Vtk.")
else ()
  message(STATUS "[FindMesa.cmake] - Did not find Mesa, use -DMesa_DIR to supply the Mesa installation directory.")
  if (SciMesa_FIND_REQUIRED)
    message(FATAL_ERROR "[FindMesa.cmake] - Failed to find Mesa.")
  endif ()
endif ()

