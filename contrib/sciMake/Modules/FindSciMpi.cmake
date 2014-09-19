# - FindSciMpi: This module looks for MPI first as being in the compilers, i.e.,
# MPI compiler wrappers, by trying to compile sample code.  It then
# looks for MPI using the standard CMake module, unless the compilers
# have the Cray names, in which case we know that this will fail.
#
# The following varialbes are set:
#
# TX_HAVE_MPICXX_COMPILER_WRAPPER: TRUE if the C++ compiler automatically
#   includes and links to MPI.  If this is named 'CC', then no further MPI
#   searching is done.
# TX_HAVE_MPIFC_COMPILER_WRAPPER: TRUE if the Fortran compiler automatically
#   includes and links to MPI.  If this is named 'ftn', then no further
#   MPI searching is done.
# TXMPI_FOUND is set to TRUE if either of the above is true.
#
# In the case where we further search using the standard MPI module,
# and that is successfule, the following additional variables are set:
#
# MPI_INCLUDE_DIRS: the directories containing the C/C++ MPI header files
# MPI_MODULE_DIRS:  the directories containing the Fortran module files
#                   and the Fortran include files
# MPI_LIBRARY_DIRS: the directories containing the MPI libraries
# MPI_EXECUTABLES:  mpiexec

######################################################################
#
# FindSciMpi: check whether the compiler wraps MPI, if not, find MPI
#
# $Id: FindSciMpi.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

set(SEARCH_FOR_MPI TRUE)
set(HAVE_MPI 0 CACHE BOOL "Whether have MPI")

# Determine whether mpi is already in the C++ compiler
try_compile(TX_HAVE_MPICXX_COMPILER_WRAPPER ${PROJECT_BINARY_DIR}/CMake
  ${TXCMAKE_DIR}/trycompile/mpi_h.cxx)
if (TX_HAVE_MPICXX_COMPILER_WRAPPER)
  message(STATUS "Using C/C++ compiler wrappers.")
# If so, set that to the compiler
  set(MPI_C_COMPILER ${CMAKE_C_COMPILER})
  set(MPI_CXX_COMPILER ${CMAKE_CXX_COMPILER})
  set(TXMPI_FOUND TRUE)
# Search for MPI libraries not possible with Cray compiler wrappers
  if (${CMAKE_CXX_COMPILER} MATCHES "CC$")
    set(SEARCH_FOR_MPI FALSE)
  endif ()
endif ()

# If have fortran determine whether mpi is already in the Fortran compiler
if (CMAKE_Fortran_COMPILER_WORKS)
  try_compile(TX_HAVE_MPIFC_COMPILER_WRAPPER ${PROJECT_BINARY_DIR}/CMake
    ${TXCMAKE_DIR}/trycompile/mpi_mod.f90)
  if (TX_HAVE_MPIFC_COMPILER_WRAPPER)
    message(STATUS "Using Fortran compiler wrapper.")
    set(MPI_Fortran_COMPILER ${CMAKE_Fortran_COMPILER})
    set(TXMPI_FOUND TRUE)
  endif ()
# Search for MPI libraries not possible with Cray compiler wrappers
  if (${CMAKE_Fortran_COMPILER} MATCHES "ftn$")
    set(SEARCH_FOR_MPI FALSE)
  endif ()
endif ()

# Pass down the required variable.  This has file name capitalization.
if (SEARCH_FOR_MPI)
  if (SciMpi_FIND_REQUIRED)
    set(mpireq REQUIRED)
  else ()
    set(mpireq)
  endif ()
  find_package(MPI ${mpireq})
endif ()

# Either gives MPI
if (MPI_FOUND OR TXMPI_FOUND)
  set(HAVE_MPI 1)
endif ()

# If know more than compiler wrappers, pull out standard values
if (MPI_FOUND)

# Fix the variables
  set(MPI_INCLUDE_DIRS ${MPI_INCLUDE_PATH})

# Get the library dirs.
# Assuming that finding fortran libs later will not affect this.
  set(MPI_LIBRARY_DIRS)
  foreach (lib ${MPI_LIBRARIES})
    get_filename_component(dir ${lib}/.. REALPATH)
    set(MPI_LIBRARY_DIRS ${MPI_LIBRARY_DIRS} ${dir})
  endforeach ()
  if (MPI_LIBRARY_DIRS)
    list(REMOVE_DUPLICATES MPI_LIBRARY_DIRS)
  endif ()

# If fortran libraries not found, look for them in the MPI_DIRS by names
  if (CMAKE_Fortran_COMPILER_WORKS AND NOT MPI_Fortran_LIBRARIES)
    foreach (libname mpi_f90 mpi_f77 msmpifec)
      find_library(MPI_${libname} ${libname} ${MPI_LIBRARY_DIRS})
      if (MPI_${libname})
        set(MPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARIES} ${MPI_${libname}})
      endif ()
    endforeach ()
# Fortran depends on C
    set(MPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARIES} ${MPI_C_LIBRARIES})
  endif ()

# Get master library list, removing duplicates
  set(mpilibs ${MPI_LIBRARIES})
  if (MPI_Fortran_LIBRARIES)
    message(STATUS "Adding fortran mpi libraries, ${MPI_Fortran_LIBRARIES}, to mpi libraries.")
    set(mpilibs ${MPI_Fortran_LIBRARIES} ${mpilibs})
  endif ()
  if (mpilibs)
    list(REVERSE mpilibs)
    list(REMOVE_DUPLICATES mpilibs)
    list(REVERSE mpilibs)
# Strip any system libs off and put final result into NAMES and LIBRARIES
    set(MPI_LIBRARIES)
    set(MPI_LIBRARY_NAMES)
    foreach (lib ${mpilibs})
      if (${lib} MATCHES "/libdl\\." OR ${lib} MATCHES "/libnsl\\." OR ${lib} MATCHES "/libutil\\." OR ${lib} MATCHES "/libm\\.")
      else ()
        set(MPI_LIBRARIES ${MPI_LIBRARIES} ${lib})
        get_filename_component(libname ${lib} NAME_WE)
        string(REGEX REPLACE "^lib" "" libname ${libname})
        set(MPI_LIBRARY_NAMES ${MPI_LIBRARY_NAMES} ${libname})
      endif ()
    endforeach ()
# Get static libs
    # message(STATUS "Getting static libraries for ${MPI_LIBRARIES}")
    SciGetStaticLibs("${MPI_LIBRARIES}" MPI_STLIBS)
  endif ()

# Assume only one include dir
  if (MPI_DIR AND MPI_INCLUDE_DIRS)
    get_filename_component(MPI_DIR ${MPI_INCLUDE_DIRS}/.. REALPATH)
  endif ()

# Get module includes
  set(MPI_MODULE_DIRS ${MPI_Fortran_INCLUDE_PATH})

# Get the executables
  set(MPI_EXECUTABLES ${MPIEXEC})

# MPI link for flags
# The string strip line is needed because cmake
# doesn't allow for leading/trailing whitespace.
#
# Kludge for strip call
  if (MPI_LINK_FLAGS)
    string(STRIP "${MPI_LINK_FLAGS}" MPI_LINK_FLAGS)
  endif ()

  message(STATUS "Enabling MPI")
  sciPrintCMakeResults("MPI")
  sciPrintVar(MPI_LINK_FLAGS)

# Pass up the found variable.  This is all caps.
  set(TXMPI_FOUND TRUE)

endif ()

if (NOT TXMPI_FOUND)
  if (SciMpi_FIND_REQUIRED)
    message(FATAL_ERROR "MPI required but not found.")
  else ()
    message(STATUS "MPI not enabled.")
  endif ()
endif ()
