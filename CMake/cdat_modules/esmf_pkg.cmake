include(CMakeDependentOption)

set(ESMF_MAJOR 6)
set(ESMF_MINOR 3)
set(ESMF_PATCH 0rp1)
set(ESMP_MAJOR 01)
set(ESMF_VERSION ${ESMF_MAJOR}_${ESMF_MINOR}_${ESMF_PATCH})
set(ESMF_URL ${LLNL_URL})
set(ESMF_GZ esmp.ESMF_${ESMF_VERSION}_ESMP_${ESMP_MAJOR}.tar.bz2)
set(ESMF_MD5  a9be4fb51da1bc1fab027137297c5030 )
set(ESMF_SOURCE ${ESMF_URL}/${ESMF_GZ})

if (CDAT_BUILD_LEAN)
    option(CDAT_BUILD_ESMF_ESMP "Build python version Earth System Modeling Framework" OFF)
else ()
    option(CDAT_BUILD_ESMF_ESMP "Build python version Earth System Modeling Framework" ON)
endif()

cmake_dependent_option(CDAT_BUILD_ESMF_PARALLEL
  "Build parallel version of Earth System Modeling Framework library" ON
  "CDAT_BUILD_PARALLEL" OFF
)

set(TXCMAKE_DIR ${cdat_SOURCE_DIR}/contrib/sciMake)
include(${TXCMAKE_DIR}/sciFuncsMacros.cmake)
include(${TXCMAKE_DIR}/sciFortranChecks.cmake)

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL GNU)
  string(REGEX MATCHALL "[0-9]+\\." test_version_list ${Fortran_VERSION})
  string(SUBSTRING ${Fortran_VERSION} 0 3 Fortran_MAJOR_VERSION)
  LIST(GET test_version_list 0 Fortran_MAJOR_VERSION)
  LIST(GET test_version_list 1 Fortran_MINOR_VERSION)
else()
  set(Fortran_MINOR_VERSION "")
endif()

if(CDAT_BUILD_ESMF_ESMP)
    if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL GNU)
      # GNU gfortran must be >= 4.3
      if(${Fortran_MAJOR_VERSION} GREATER 3 AND ${Fortran_MINOR_VERSION} GREATER 2)
        ## On APPLE need to test for -arch as well!
        add_cdat_package(ESMF "" "Build ESMF" ON)
      else()
        message("[INFO] Skipping ESMF")
        message("[INFO] gfortran version needs to be at least 4.3 to install ESMF")
        message("[INFO] You have ${Fortran_VERSION}")
      endif()
    else()
      add_cdat_package(ESMF "" "Build ESMF" ON)
      message("[INFO] Fortran Compiler is: ${CMAKE_Fortran_COMPILER}")
    endif()

     # the following may need to be adjusted on Crays, otherwise the defaults will likely apply
     set(CDAT_BUILD_ESMF_OS "${CMAKE_SYSTEM_NAME}" CACHE STRING "ESMF_OS env variable, may need to change to Unicos on Crays")
     set(CDAT_BUILD_ESMF_COMPILER "gfortran" CACHE STRING "ESMF_COMPILER env variable, choices are gfortran, intel, pgi, g95, or nag")
     set(CDAT_BUILD_ESMF_COMM "openmpi" CACHE STRING "ESMF_COMM env variable, choices are openmpi, mpiuni, mpi, mpich2, or mvapich2")
     set(CDAT_BUILD_ESMF_ABI "64" CACHE STRING "ESMF_ABI env variable, choices are 32 or 64")
endif()

