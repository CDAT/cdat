# - FindSciCubit: Module to find include directories and
#   libraries for Cubit.
#
# Module usage:
#   find_package(SciCubit ...)
#
# This module will define the following variables:
#  HAVE_CUBIT, CUBIT_FOUND = Whether libraries and includes are found
#  Cubit_INCLUDE_DIRS        = Location of Cubit includes
#  Cubit_LIBRARY_DIRS        = Location of Cubit libraries
#  Cubit_LIBRARIES           = Required libraries

##################################################################
#
# Find module for CUBIT
#
# $Id: FindSciCubit.cmake 1238 2012-01-30 19:19:13Z mdurant $
#
##################################################################

set(Cubit_LIBRARY_LIST
  CMLTet
  SpacACIS
  cbtverdict
  cubit_geom
  cubit_util
  cubitgui
  cubiti19
  gtcAttrib
  libifcoremd
  libifportmd
  libmmd
  lpsolve55
  mesquite
)

SciFindPackage(
  PACKAGE Cubit
  INSTALL_DIR cubit
  HEADERS CubitGUIInterface.hpp
  LIBRARIES "${Cubit_LIBRARY_LIST}"
)

if (CUBIT_FOUND)
  message(STATUS "[FindSciCubit.cmake] - Found Cubit")
  message(STATUS "[FindSciCubit.cmake] - Cubit_INCLUDE_DIRS = ${Cubit_INCLUDE_DIRS}")
  message(STATUS "[FindSciCubit.cmake] - Cubit_LIBRARIES = ${Cubit_LIBRARIES}")
  set(HAVE_CUBIT 1 CACHE BOOL "Whether have Cubit.")
else ()
  message(STATUS "[FindSciCubit.cmake] - Did not find Cubit, use -DCUBIT_DIR to supply the CUBIT installation directory.")
  if (SciCubit_FIND_REQUIRED)
    message(FATAL_ERROR "[FindSciCubit.cmake] - Failing.")
  endif ()
endif ()

