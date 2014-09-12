# - FindSciChombo: Module to find include directories and
#   libraries for Chombo.
#
# Module usage:
#   find_package(SciChombo ...)
#
# This module will define the following variables:
#  HAVE_CHOMBO, CHOMBO_FOUND = Whether libraries and includes are found
#  Chombo_INCLUDE_DIRS       = Location of Chombo includes
#  Chombo_LIBRARY_DIRS       = Location of Chombo libraries
#  Chombo_LIBRARIES          = Required libraries

######################################################################
#
# SciFindChombo: find includes and libraries for Chombo.
#
# $Id: FindSciChombo.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################
if (CH_SPACEDIM)
  set(CH_SPACEDIM_SUFFIX "${CH_SPACEDIM}d")
else ()
  # default 2d
  set(CH_SPACEDIM_SUFFIX "2d")
endif ()

if (CH_DEBUG)
  set(CH_DEBUG_SUFFIX ".DEBUG")
endif ()

if (CHOMBO_ARCH MATCHES 32)
  set(CH_ARCH_SUFFIX ${CHOMBO_ARCH})
else ()
  set(CH_ARCH_SUFFIX ".64")
endif ()

if (CH_OPT)
  set(CH_OPT_SUFFIX ".OPT")
endif ()

if (CH_PROF)
  set(CH_PROF_SUFFIX ".PROF")
endif ()

if (ENABLE_PARALLEL)
  set(CH_MPI_SUFFIX ".MPI")
  set(CH_MPI "1")
endif ()

if (NOT CH_CXX)
  if (ENABLE_PARALLEL)
    set(REL_CMAKE_CXX_COMPILER "mpicxx")
  else ()
    get_filename_component(REL_CMAKE_CXX_COMPILER ${CMAKE_CXX_COMPILER} NAME)
  endif ()
else ()
  set(REL_CMAKE_CXX_COMPILER ${CH_CXX})
endif ()

if (NOT CH_FC)
  if (ENABLE_PARALLEL)
    set(REL_CMAKE_Fortran_COMPILER "mpif90")
  else ()
    get_filename_component(REL_CMAKE_Fortran_COMPILER ${CMAKE_Fortran_COMPILER} NAME)
  endif ()
else ()
  set(REL_CMAKE_Fortran_COMPILER ${CH_FC})
endif ()

set(suffix "${CH_SPACEDIM_SUFFIX}.${CMAKE_SYSTEM_NAME}${CH_ARCH_SUFFIX}.${REL_CMAKE_CXX_COMPILER}.${REL_CMAKE_Fortran_COMPILER}${CH_DEBUG_SUFFIX}${CH_OPT_SUFFIX}${CH_PROF_SUFFIX}${CH_MPI_SUFFIX}")

set(CHOMBO_LIBRARY_LIST
  "${CHOMBO_LIBRARY_LIST}"
  "ebamrelliptic${suffix}"
  "ebamrtimedependent${suffix}"
  "ebamrtools${suffix}"
  "ebtools${suffix}"
  "workshop${suffix}"
  "amrelliptic${suffix}"
  "amrtimedependent${suffix}"
  "amrtools${suffix}"
  "boxtools${suffix}"
  "basetools${suffix}"
)

set(CHOMBO_SHORTCUT)

#if(ENABLE_PARALLEL)
#  set(CHOMBO_SHORTCUT "chombo-3.1.0-par${CH_DEBUG_DIR}${CH_SPACEDIM}d")
#else()
#  set(CHOMBO_SHORTCUT "chombo-3.1.0-ser${CH_DEBUG_DIR}${CH_SPACEDIM}d")
#endif()

message(STATUS "Looking for Chombo with directory name ${CHOMBO_SHORTCUT}")

SciFindPackage(PACKAGE "Chombo"
              INSTALL_DIR "chombo"
              HEADERS "Box.H;CH_assert.H;CH_HDF5.H"
              LIBRARIES ${CHOMBO_LIBRARY_LIST}
              )

if (CHOMBO_FOUND)
  message(STATUS "Found Chombo")
  set(HAVE_CHOMBO 1 CACHE BOOL "Whether have the Chombo library")
else ()
  message(STATUS "Did not find Chombo.  Use -DCHOMBO_DIR to specify the installation directory.")
  if (SciChombo_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()
