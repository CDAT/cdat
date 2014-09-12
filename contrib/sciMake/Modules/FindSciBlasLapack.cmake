# - FindSciBlasLapack: find includes and libraries for lapack and blas
#
# This is a work in progress.  The goal is to find Blas and Lapack.
# Since we make use of the standard CMake find packages, variables
# are all CAPS, rather than CamelCaps as dictated by Tech-X conventions.
#
# This should be included after either Trilinos or PETSc is found,
# if they are used, so that their third party libraries can be used
# to set Blas and Lapack.
#
# The determining invocation args are
#  -DLAPACK_LIBRARIES:PATH="<lapacklibs>" -DBLAS_LIBRARIES:PATH="<blaslibs>"
#
# One can also set
#  -DLAPACK_LIBRARY_DIRS:PATH='<lapackdirs>' -DLAPACK_LIBRARY_NAMES:STRING='lapacknames' -DBLAS_LIBRARY_DIRS:PATH='<blasdirs>' -DBLAS_LIBRARY_NAMES:STRING='blasnames'
#
# If the libraries are still not found, we use the following search order
# Optionally: use whatever trilinos linked against
#   if trilinos found.(This is the VORPAL preference.)
# Optionally: use whatever PETSc linked against if PETSc
#   found.(This is the FACETS preference.)
# System optimized(use FCLIBS or equiv to help link):
#   -framework Accelerate
#   essl
#   mkl
#   acml
#   /contrib/atlas-ser(the one built againt lapack)
#   /contrib/altas-clp(the one built against clapack)
#   Other ATLAS in system directories
#   Other system lapack-blas in system directories.
#   Optionally GOTO(as it has a funky license)
# lapack in /contrib
# clapack in /contrib
#

######################################################################
#
# FindSciBlasLapack.cmake: find includes and libraries Blas and Lapack
#
# $Id: FindSciBlasLapack.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

include(${TXCMAKE_DIR}/SciSeparateLibs.cmake)
# Heading
message("")
message("--------- FindSciBlasLapack looking for Blas and Lapack ---------")

# Apple is a special case
if (APPLE)
  find_library(ACCELERATE_FRAMEWORK Accelerate)
endif ()

# Allow specification of
#   LAPACK_LIBRARIES
#     OR
#   (LAPACK_LIBRARY_NAMES AND (LAPACK_LIBRARY_DIRS OR LAPACK_DIRS))
#   and then find the other
if (APPLE)
  set(LAPACK_LIBRARIES ${ACCELERATE_FRAMEWORK})
elseif (LAPACK_LIBRARIES)
  if (NOT(LAPACK_LIBRARY_DIRS AND LAPACK_LIBRARY_NAMES))
    SciSeparateLibs(${LAPACK_LIBRARIES} LAPACK_LIBFLAGS LAPACK_LIBRARY_DIRS LAPACK_LIBRARY_NAMES)
  endif ()
else ()
  if (NOT LAPACK_LIBRARY_NAMES)
    set(LAPACK_LIBRARY_NAMES_SAV ${LAPACK_LIBRARY_NAMES})
    set(LAPACK_LIBRARY_NAMES lapack)
  endif ()
  if (LAPACK_DIRS)
    set(sfxs lib)
  elseif (LAPACK_LIBRARY_DIRS)
    set(sfxs)
    foreach (libdir ${LAPACK_LIBRARY_DIRS})
      get_filename_component(dir ${libdir}/.. REALPATH)
      set(LAPACK_DIRS ${LAPACK_DIRS} ${dir})
      get_filename_component(sfx ${libdir} NAME)
      set(sfxs ${sfxs} ${sfx})
    endforeach ()
  endif ()
  message("LAPACK_DIRS = ${LAPACK_DIRS}.")
  foreach (libname ${LAPACK_LIBRARY_NAMES})
    if (LAPACK_DIRS)
      # message("Looking for ${libname} in ${LAPACK_DIRS} / ${sfxs}.")
      find_library(mylapacklib ${libname} PATHS ${LAPACK_DIRS}
        PATH_SUFFIXES ${sfxs}
        NO_DEFAULT_PATH
      )
      # message("Result: mylapacklib = ${mylapacklib}.")
    else ()
      find_library(mylapacklib ${libname})
    endif ()
    if (mylapacklib)
      set(LAPACK_LIBRARIES ${LAPACK_LIBRARIES} ${mylapacklib})
    endif ()
  endforeach ()
  if (NOT LAPACK_LIBRARIES)
    set(LAPACK_LIBRARY_NAMES ${LAPACK_LIBRARY_NAMES_SAV})
  endif ()
endif ()
if (LAPACK_LIBRARIES)
  SciGetStaticLibs("${LAPACK_LIBRARIES}" LAPACK_STLIBS)
endif ()

# Allow specification of
#   BLAS_LIBRARIES
#     OR
#   (BLAS_LIBRARY_NAMES AND (BLAS_LIBRARY_DIRS OR BLAS_DIRS))
#   and then find the other
if (APPLE)
  set(BLAS_LIBRARIES ${ACCELERATE_FRAMEWORK})
elseif (BLAS_LIBRARIES)
  if (NOT(BLAS_LIBRARY_DIRS AND BLAS_LIBRARY_NAMES))
    #SciSeparateLibs(${LAPACK_LIBRARIES} LAPACK_LIBFLAGS LAPACK_LIBRARY_DIRS LAPACK_LIBRARY_NAMES LAPACK_LIBRARY_DUMMY)
    SciSeparateLibs(${BLAS_LIBRARIES} BLAS_LIBFLAGS LAPACK_LIBRARY_LIBRARIES BLAS_LIBRARY_DIRS BLAS_LIBRARY_NAMES)
  endif ()
else ()
  if (NOT BLAS_LIBRARY_NAMES)
    set(BLAS_LIBRARY_NAMES_SAV ${BLAS_LIBRARY_NAMES})
    set(BLAS_LIBRARY_NAMES blas)
  endif ()
  if (BLAS_DIRS)
    set(sfxs lib)
  elseif (BLAS_LIBRARY_DIRS)
    set(sfxs)
    foreach (libdir ${BLAS_LIBRARY_DIRS})
      get_filename_component(dir ${libdir}/.. REALPATH)
      set(BLAS_DIRS ${BLAS_DIRS} ${dir})
      get_filename_component(sfx ${libdir} NAME)
      set(sfxs ${sfxs} ${sfx})
    endforeach ()
  endif ()
  # message("BLAS_DIRS = ${BLAS_DIRS}.")
  foreach (libname ${BLAS_LIBRARY_NAMES})
    if (BLAS_DIRS)
      find_library(myblaslib ${libname} PATHS ${BLAS_DIRS}
        PATH_SUFFIXES ${sfxs}
        NO_DEFAULT_PATH
      )
    else ()
      find_library(myblaslib ${libname})
      # message("Result: myblaslib = ${myblaslib}.")
    endif ()
    if (myblaslib)
      set(BLAS_LIBRARIES ${BLAS_LIBRARIES} ${myblaslib})
    endif ()
  endforeach ()
  if (NOT BLAS_LIBRARIES)
    set(BLAS_LIBRARY_NAMES ${BLAS_LIBRARY_NAMES_SAV})
  endif ()
endif ()
if (BLAS_LIBRARIES)
  SciGetStaticLibs("${BLAS_LIBRARIES}" BLAS_STLIBS)
endif ()

# Start with the CMake module
# if (NOT LAPACK_LIBRARIES)
#   find_package(LAPACK ${LAPACK_REQUIRED})
# endif ()
# if (NOT BLAS_LIBRARIES)
#   find_package(BLAS ${BLAS_REQUIRED})
# endif ()
message(STATUS "")
sciPrintCMakeResults(LAPACK)
message(STATUS "")
sciPrintCMakeResults(BLAS)

