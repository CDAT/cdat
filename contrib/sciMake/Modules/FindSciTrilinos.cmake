# - FindSciTrilinos: Module to find include directories and
#   libraries for Trilinos.
#
# Module usage:
#   find_package(SciTrilinos ...)
#
# If USE_TRILINOS_CONFIG_CMAKE is TRUE, use the supra-search-path
# to find TrilinosConfig.cmake, include it, then translate to our
# variable conventions:
#
# Otherwise use SciFindPackage.
#
# In either case, the following variables get defined.
#
# Trilinos_DIR          : root installation directory for Trilinos
# Trilinos_EXECUTABLES  : any Trilinos executables (empty for trilinos)
# Trilinos_FILES        : any other files (empty for trilinos)
# Trilinos_INCLUDE_DIRS : include directories needed for compiling C/C++
# Trilinos_MODULE_DIRS  : include directories needed for compiling fortran
# Trilinos_LIBFLAGS     : any flags (except rpath flags) needed for linking the
#                         trilinos libraries
# Trilinos_LIBRARY_DIRS : the directories containing the libraries
# Trilinos_LIBRARY_NAMES: the base library names, no extensions, no prefix
# Trilinos_LIBRARIES    : the full paths to the libraries
# Trilinos_STLIBS       : the full paths to the static libs if found, otherwise
#                         the same as in the above variable
# Trilinos_TPL_LIBRARIES: third-party libraries needed for linking to Trilinos
#
# and separate those into
#
#  Trilinos_LINALG_LIBRARIES: the blas and lapack libraries used by Trilinos
#  Trilinos_SYSTEM_LIBRARIES: any other system libraries needed to link Trilinos

######################################################################
#
# FindSciTrilinos: find includes and libraries for Trilinos
#
# $Id: FindSciTrilinos.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (ENABLE_PARALLEL)
  set(trilinosdir trilinos-par)
else ()
  set(trilinosdir trilinos)
endif ()

if (USE_TRILINOS_CONFIG_CMAKE)

  message("---------  Looking for Trilinos --------- ")

# Look for TrilinosConfig.cmake in our installation directories
  find_file(TRILINOS_CONFIG_CMAKE TrilinosConfig.cmake
    PATHS ${SUPRA_SEARCH_PATH}
    PATH_SUFFIXES ${trilinosdir}/lib/cmake/Trilinos ${trilinosdir}/include
      ${trilinosdir} lib/cmake/Trilinos include
    NO_DEFAULT_PATH
  )
# If not found, look in system directories
  if (NOT TRILINOS_CONFIG_CMAKE)
    find_file(TRILINOS_CONFIG_CMAKE TrilinosConfig.cmake)
  endif ()
# Print result
  if (TRILINOS_CONFIG_CMAKE)
    get_filename_component(TRILINOS_CONFIG_CMAKE ${TRILINOS_CONFIG_CMAKE} REALPATH)
    sciPrintString("TRILINOS_CONFIG_CMAKE = ${TRILINOS_CONFIG_CMAKE}.")
    set(TRILINOS_FOUND TRUE)
  else ()
    sciPrintString("TrilinosConfig.cmake not found.")
  endif ()

# If found, fix some variables
  if (TRILINOS_CONFIG_CMAKE)
# Get variables from that file
    set(TRILINOS_FOUND TRUE)
    include(${TRILINOS_CONFIG_CMAKE})
# Get our other variables
    get_filename_component(Trilinos_DIR ${Trilinos_LIBRARY_DIRS}/.. REALPATH)
# Translate library variables to our conventions and get static libraries
    set(Trilinos_LIBRARY_NAMES ${Trilinos_LIBRARIES})
    set(Trilinos_LIBRARIES)
    foreach (libname ${Trilinos_LIBRARY_NAMES})
      find_library(${libname}_LIBRARY ${libname} ${Trilinos_LIBRARY_DIRS} NO_DEFAULT_PATH)
      if (${libname}_LIBRARY)
        set(Trilinos_LIBRARIES ${Trilinos_LIBRARIES} ${${libname}_LIBRARY})
      endif ()
    endforeach ()
    SciGetStaticLibs("${Trilinos_LIBRARIES}" Trilinos_STLIBS)
  endif ()

# Print out the results
  sciPrintCMakeResults(Trilinos)
else () # Use SciFindPackage

  SciFindPackage(PACKAGE "Trilinos"
    INSTALL_DIR ${trilinosdir}
    HEADERS "az_aztec.h"
    LIBRARIES "aztecoo;ml;zoltan;amesos;ifpack;epetraext;galeri;triutils;epetra;shards;intrepid;komplex;teuchos"
  )
# Above sets TRILINOS_FOUND to TRUE?

# Find the blas and lapack used by Trilinos.
  if (TRILINOS_FOUND)
    find_file(TRILINOS_CONFIG_CMAKE
      TrilinosConfig.cmake
      PATHS ${Trilinos_DIR}/lib/cmake/Trilinos
            ${Trilinos_az_aztec_h_INCLUDE_DIR} ${Trilinos_DIR}
    )
    if (NOT TRILINOS_CONFIG_CMAKE)
      message(FATAL_ERROR "TRILINOS_CONFIG_CMAKE not found.")
    endif ()
    if (DEBUG_CMAKE)
      message(STATUS "TRILINOS_CONFIG_CMAKE ${TRILINOS_CONFIG_CMAKE}.")
    endif ()
  # Cannot include ${TRILINOS_CONFIG_CMAKE} as overwrites some definitions,
  # so must parse out the variable.
    file(STRINGS ${TRILINOS_CONFIG_CMAKE} tritpls
        REGEX ".*Trilinos_TPL_LIBRARIES.*")
      # message("tritpls = ${tritpls}")
  # Remove definition line
    string(REGEX REPLACE "SET.Trilinos_TPL_LIBRARIES ." "" tritpls "${tritpls}")
      # message("tritpls = ${tritpls}")
  # Remove trailing quote and paren
    string(REGEX REPLACE ".\\)$" "" tritpls "${tritpls}")
      # message("tritpls = ${tritpls}")
  # Remove exterior white space
    string(STRIP ${tritpls} tritpls)
      # message("tritpls = ${tritpls}")
  # Make this a string list.
  # JRC: No can do.  On Windows, strings have blanks.
      # separate_arguments(Trilinos_TPL_LIBRARIES)
    string(REGEX REPLACE "\\\\;" ";" Trilinos_TPL_LIBRARIES "${tritpls}")
  endif ()

endif ()

if (TRILINOS_FOUND)
# Should now have all standard variables plus Trilinos_TPL_LIBRARIES

# Some options
  option(HAVE_AMESOS "Trilinos Amesos Library" ON)
  option(HAVE_EPETRAEXT "Trilinos Epetraext library" ON)
  option(HAVE_TRILINOS "Trilinos libraries" ON)

# Remove duplicates
  list(REVERSE Trilinos_TPL_LIBRARIES)
  list(REMOVE_DUPLICATES Trilinos_TPL_LIBRARIES)
  list(REVERSE Trilinos_TPL_LIBRARIES)

# Separate the third-party libraries into blas/lapack and system libraries
  set(Trilinos_LINALG_LIBRARIES)
  set(Trilinos_MPI_LIBRARIES)
  set(Trilinos_SYSTEM_LIBRARIES)
  foreach (lib ${Trilinos_TPL_LIBRARIES})
    get_filename_component(libname ${lib} NAME_WE)
    if (${libname} MATCHES "blas$" OR ${libname} MATCHES "lapack$" OR
        ${libname} MATCHES "acml$" OR ${libname} MATCHES "mkl$" OR
        ${libname} MATCHES "f2c$" OR ${libname} MATCHES "atlas$"
    )
      set(Trilinos_LINALG_LIBRARIES ${Trilinos_LINALG_LIBRARIES} ${lib})
    elseif (${libname} MATCHES "msmpi$")
      set(Trilinos_MPI_LIBRARIES ${Trilinos_MPI_LIBRARIES} ${lib})
    else ()
      set(Trilinos_SYSTEM_LIBRARIES ${Trilinos_SYSTEM_LIBRARIES} ${lib})
    endif ()
  endforeach ()
# Find the libdirs of all groups
  foreach (grp TPL LINALG MPI SYSTEM)
    set(libs ${Trilinos_${grp}_LIBRARIES})
    unset(Trilinos_${grp}_LIBRARY_DIRS)
    unset(Trilinos_${grp}_LIBRARY_NAMES)
    foreach (lib ${libs})
      get_filename_component(libdir ${lib}/.. REALPATH)
      list(APPEND Trilinos_${grp}_LIBRARY_DIRS ${libdir})
      get_filename_component(libname ${lib} NAME_WE)
      string(REGEX REPLACE "^lib" "" libname ${libname})
      list(APPEND Trilinos_${grp}_LIBRARY_NAMES ${libname})
    endforeach ()
    if (Trilinos_${grp}_LIBRARY_DIRS)
      list(REMOVE_DUPLICATES Trilinos_${grp}_LIBRARY_DIRS)
    endif ()
    # message(STATUS "  Trilinos_${grp}_LIBRARY_DIRS = ${Trilinos_${grp}_LIBRARY_DIRS}")
  endforeach ()

# Find the other libraries for the case of clapack_cmake
  find_library(F2C_LIBRARY f2c PATHS ${Trilinos_LINALG_LIBRARY_DIRS} NO_DEFAULT_PATH)
  if (F2C_LIBRARY)
    message(STATUS "  F2C_LIBRARY = ${F2C_LIBRARY}.")
# In this case, we need the clapack_cmake blas as well
    get_filename_component(libdir ${F2C_LIBRARY}/.. REALPATH)
    find_library(BLAS_LIBRARY blas PATHS ${libdir} NO_DEFAULT_PATH)
    if (BLAS_LIBRARY)
      message(STATUS "  BLAS_LIBRARY found.")
      set(Trilinos_TPL_LIBRARIES ${Trilinos_TPL_LIBRARIES} ${BLAS_LIBRARY})
      set(Trilinos_LINALG_LIBRARIES ${Trilinos_LINALG_LIBRARIES} ${BLAS_LIBRARY})
    else ()
      message(STATUS "  BLAS_LIBRARY not found.")
    endif ()
    set(Trilinos_TPL_LIBRARIES ${Trilinos_TPL_LIBRARIES} ${F2C_LIBRARY})
    set(Trilinos_LINALG_LIBRARIES ${Trilinos_LINALG_LIBRARIES} ${F2C_LIBRARY})
  else ()
    message(STATUS "  F2C_LIBRARY not found.")
  endif ()

# Final calculations
  foreach (grp TPL LINALG MPI SYSTEM)
    # if (Trilinos_${grp}_LIBRARIES)
      # list(REMOVE_DUPLICATES Trilinos_TPL_LIBRARIES)
    # endif ()
    SciGetStaticLibs("${Trilinos_${grp}_LIBRARIES}" Trilinos_${grp}_STLIBS)
    sciPrintVar(Trilinos_${grp}_LIBRARY_DIRS)
    sciPrintVar(Trilinos_${grp}_LIBRARY_NAMES)
    sciPrintVar(Trilinos_${grp}_LIBRARIES)
    sciPrintVar(Trilinos_${grp}_STLIBS)
  endforeach ()

# Combine
  if (0)
  set(Trilinos_LIBS ${Trilinos_LIBRARIES}
      "${Trilinos_TPL_LIBRARIES}" ${Fortran_IMPLICIT_LIBFLAGS})
  message(STATUS "[FindSciTrilinos]: Trilinos_LIBS = ${Trilinos_LIBS}")

# Static versions
  SciGetStaticLibs("${Trilinos_LIBS}" Trilinos_STLIBS)
  message(STATUS "[FindSciTrilinos]: Trilinos_STLIBS = ${Trilinos_STLIBS}")
  endif ()

else ()

  message(STATUS "Did not find Trilinos. Use -DTRILINOS_DIR to specify the installation directory.")

endif ()
