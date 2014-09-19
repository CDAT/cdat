# - FindSciPetsc: Module to find include directories and
#   libraries for Petsc.
#
# Module usage:
#   find_package(SciPetsc ...)
#
# This module will define the following variables:
#  HAVE_PETSC, PETSC_FOUND = Whether libraries and includes are found
#  Petsc_INCLUDE_DIRS       = Location of Petsc includes
#  Petsc_LIBRARY_DIRS       = Location of Petsc libraries
#  Petsc_LIBRARIES          = Required libraries

######################################################################
#
# SciFindPetsc: find includes and libraries for petsc.  Complex
# due to the many libraries that petsc builds
#
# $Id: FindSciPetsc.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

set(Petsc_LIBRARY_LIST
  petscts
  petscsnes
  petscksp
  petscdm
  petscmat
  petscvec
  petsc
  cmumps
  dmumps
  smumps
  zmumps
  mumps_common
  pord
  scalapack
  blacs
  superlu_dist_2.3
  superlu_dist_2.4
  superlu_4.0
  HYPRE
  parmetis
  metis
)

if (ENABLE_PARALLEL)
  SciFindPackage(PACKAGE "Petsc"
    INSTALL_DIR petsc-par
    HEADERS petsc.h
    LIBRARIES ${Petsc_LIBRARY_LIST}
  )
else ()
  SciFindPackage(PACKAGE "Petsc"
    INSTALL_DIR petsc
    HEADERS petsc.h mpi.h
    INCLUDE_SUBDIRS include include/mpiuni
    LIBRARIES ${Petsc_LIBRARY_LIST}
  )
endif ()

if (PETSC_FOUND)
  message(STATUS "Found Petsc")
  set(HAVE_PETSC 1 CACHE BOOL "Whether have the PETSC library")
else ()
  message(STATUS "Did not find Petsc.  Use -DPETSC_DIR to specify the installation directory.")
  if (Petsc_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()

########################################################################
#
# Find Petsc paths
# We are creating a temporary Makefile.petsc which includes the relevant
# petsc files. The macro petsc_get_variable, then probes this makefile
# to determine various arguments
#
#######################################################################

# Need PETSC_DIR for the CMake/Makefile.show
get_filename_component(Petsc_DIR ${Petsc_petsc_h_INCLUDE_DIR} PATH)
set(PETSC_DIR ${Petsc_DIR})

# Get the make command
find_program(MAKE_EXECUTABLE NAMES make gmake)

# Get Petsc package variables that have a target
macro(PETSC_GET_TARGET_VARIABLE name var)
  set(${var} "NOTFOUND" CACHE INTERNAL "Cleared" FORCE)
  execute_process(COMMAND ${MAKE_EXECUTABLE} PETSC_DIR=${PETSC_DIR}
    -f "${TXCMAKE_DIR}/Makefile.show" ${name}
    OUTPUT_VARIABLE ${var}
    OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE return)
endmacro(PETSC_GET_TARGET_VARIABLE)

# Get other Petsc Package variables from a Petsc Makefile
macro(PETSC_GET_VARIABLE name var)
  set(${var} "NOTFOUND" CACHE INTERNAL "Cleared" FORCE)
  execute_process(COMMAND ${MAKE_EXECUTABLE} PETSC_DIR=${PETSC_DIR}
    --no-print-directory -f "${TXCMAKE_DIR}/Makefile.show"
    showvar PKG_VARIABLE=${name}
    OUTPUT_VARIABLE ${var}
    RESULT_VARIABLE return
  )
endmacro(PETSC_GET_VARIABLE)

# getlinklibs is needed for some projects
petsc_get_target_variable(getlinklibs Petsc_LINKER_LIBS)
message(STATUS "  Petsc_LINKER_LIBS  = ${Petsc_LINKER_LIBS}.")
set(Petsc_LINKER_LIBLIST "${Petsc_LINKER_LIBS}")
SciMakeLibList(Petsc_LINKER_LIBLIST)
message(STATUS "  Petsc_LINKER_LIBLIST  = ${Petsc_LINKER_LIBLIST}.")

# X11_LIB is needed when building Petsc with Zoltan
# grab this variable with the macro that uses "make showvar"
petsc_get_variable(X11_LIB Petsc_LIB_X11)
message(STATUS "  Petsc_LIB_X11      = ${Petsc_LIB_X11}.")
set(Petsc_X11_LIBLIST "${Petsc_X11_LIBS}")
SciMakeLibList(Petsc_X11_LIBLIST)
message(STATUS "  Petsc_X11_LIBLIST  = ${Petsc_X11_LIBLIST}.")

# Get the libs that petsc needs to link
petsc_get_variable(PCC_LINKER_LIBS PCC_LINKER_LIBS)
message(STATUS "  PCC_LINKER_LIBS    = ${PCC_LINKER_LIBS}.")
set(PCC_LINKER_LIBLIST "${PCC_LINKER_LIBS}")
SciMakeLibList(PCC_LINKER_LIBLIST)
message(STATUS "  PCC_LINKER_LIBLIST  = ${PCC_LINKER_LIBLIST}.")

########################################################################
#
# Create proper CMake flags and libs
#
########################################################################

# PETSc has libraries like superlu, for which NAME_WE does not
# work, so we will just try to get these from Petsc_LINKER_LIBS
message("")
message("--------- Recomputing Petsc libs from Petsc_LINKER_LIBS -----------")
list(REVERSE Petsc_LINKER_LIBLIST)
list(REMOVE_DUPLICATES Petsc_LINKER_LIBLIST)
list(REVERSE Petsc_LINKER_LIBLIST)
sciPrintVar(Petsc_LINKER_LIBLIST)
SciSeparateLibs("${Petsc_LINKER_LIBLIST}" Petsc_All_FLAGS Petsc_All_LIBRARIES
  Petsc_All_LIBRARY_DIRS Petsc_All_LIBRARY_NAMES Petsc_All_FRAMEWORKS)
foreach (var Petsc_All_FLAGS Petsc_All_LIBRARIES
    Petsc_All_LIBRARY_DIRS Petsc_All_LIBRARY_NAMES Petsc_All_FRAMEWORKS)
  sciPrintVar(${var})
endforeach ()

# Separate out the libraries into groups
set(Petsc_MPI_LIBRARY_NAMES)
set(Petsc_LINALG_LIBRARY_NAMES)
set(Petsc_SYSTEM_LIBRARY_NAMES)
foreach (i ${Petsc_All_LIBRARY_NAMES})
  set(libfound FALSE)
# Ignore for PETSc libraries
  list(FIND Petsc_LIBRARY_NAMES ${i} indx)
  # message(STATUS "For ${i} indx = ${indx}.")
  if (indx GREATER -1)
    # message(STATUS "${i} is a Petsc library.  Ignoring.")
    set(libfound TRUE)
  endif ()

# Ignore fortran libraries
  if (NOT libfound)
    if (${i} STREQUAL "gfortran")
      message(STATUS "${i} is an ignored fortran library.")
      set(libfound TRUE)
    endif ()
  endif ()

# Ignore some system libraries
  if (NOT libfound)
    if (${i} STREQUAL "rt" OR ${i} STREQUAL "m" OR
        ${i} STREQUAL "stdc++" OR ${i} STREQUAL "util" OR
        ${i} STREQUAL "pthread" OR ${i} STREQUAL "dl")
      message(STATUS "${i} is an ignored system library.")
      set(libfound TRUE)
    endif ()
  endif ()

# Pull out MPI libraries
  if (NOT libfound)
    if (${i} MATCHES "^mpi_" OR ${i} STREQUAL "mpi" OR
        ${i} STREQUAL "mpich" OR ${i} MATCHES "^open-" OR ${i} STREQUAL "nsl")
      message(STATUS "${i} is an mpi library.")
      set(Petsc_MPI_LIBRARY_NAMES ${Petsc_MPI_LIBRARY_NAMES} ${i})
      set(libfound TRUE)
    endif ()
  endif ()

# Pull out linear algebra libraries
  if (NOT libfound)
    if (${i} STREQUAL "blas" OR ${i} STREQUAL "lapack" OR ${i} STREQUAL "acml" OR ${i} STREQUAL "mkl")
      message(STATUS "${i} is a linear algebra library.")
      set(Petsc_LINALG_LIBRARY_NAMES ${Petsc_LINALG_LIBRARY_NAMES} ${i})
      set(libfound TRUE)
    endif ()
  endif ()

# System libraries
  if (NOT libfound)
    set(Petsc_SYSTEM_LIBRARY_NAMES ${Petsc_SYSTEM_LIBRARY_NAMES} ${i})
  endif ()

endforeach ()

# Find the ext libraries
foreach (vartype MPI LINALG SYSTEM)
  foreach (lib ${Petsc_${vartype}_LIBRARY_NAMES})
    find_library(${lib}_LIBRARY ${lib} ${Petsc_${vartype}_LIBRARY_DIRS} ${Petsc_ALLEXT_LIBRARY_DIRS} NO_DEFAULT_PATH)
    if (NOT ${lib}_LIBRARY)
      find_library(${lib}_LIBRARY ${lib} ${Petsc_${vartype}_LIBRARY_DIRS} ${Petsc_ALLEXT_LIBRARY_DIRS})
    endif ()
    if (${lib}_LIBRARY)
      set(Petsc_${vartype}_LIBRARIES ${Petsc_${vartype}_LIBRARIES} ${${lib}_LIBRARY})
      get_filename_component(libdir ${${lib}_LIBRARY}/.. REALPATH)
      set(Petsc_${vartype}_LIBRARY_DIRS ${Petsc_${vartype}_LIBRARY_DIRS} ${libdir})
    endif ()
  endforeach ()
  if (Petsc_${vartype}_LIBRARY_DIRS)
    list(REMOVE_DUPLICATES Petsc_${vartype}_LIBRARY_DIRS)
  endif ()
  SciGetStaticLibs("${Petsc_${vartype}_LIBRARIES}" Petsc_${vartype}_STLIBS)
endforeach ()

# Print all out
foreach (vartype MPI LINALG SYSTEM)
  foreach (var LIBRARY_NAMES LIBRARY_DIRS LIBRARIES STLIBS)
    sciPrintvar(Petsc_${vartype}_${var})
  endforeach ()
endforeach ()
