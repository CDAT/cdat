######################################################################
#
# sciFortranFindVersion: Determine compiler version for any compiler
#
# $Id: sciFortranFindVersion.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

sciPrintString("CMAKE_Fortran_COMPILER_ID = '${CMAKE_Fortran_COMPILER_ID}'.")
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL GNU)
# exec_program is deprecated
  execute_process(
    COMMAND ${CMAKE_Fortran_COMPILER} --version
    OUTPUT_VARIABLE fc_version_str
  )
# Test for ubuntu
  # set(fc_version_str "gcc(Ubuntu 4.4.3-4ubuntu5) 4.4.3 ")
  # message(STATUS "fc_version_str = '${fc_version_str}'")
# Works on stix: '(GCC) m.n.r' no trailing space on stix
  # string(REGEX MATCH '(GCC) [0-9]+\\.[0-9]+\\.[0-9]+'
  string(REGEX MATCH ".GCC. [0-9]+\\.[0-9]+\\.[0-9]+"
    fc_version_tmp "${fc_version_str}"
  )
  # message(STATUS "fc_version_tmp = '${fc_version_tmp}'")
  if (NOT fc_version_tmp)
# Works for Ubuntu
    string(REGEX MATCH " [0-9]+\\.[0-9]+\\.[0-9]+ *\$"
      fc_version_tmp "${fc_version_str}"
    )
  endif ()
  if (NOT fc_version_tmp)
# Now try to get Gentoo...not very robust either, but nothing else is working.
# Could do with more sophisticated options such as multiline regex/ \s
    string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+"
      fc_version_tmp "${fc_version_str}"
    )
  endif ()
  # message(STATUS "fc_version_tmp = '${fc_version_tmp}'")
  if (NOT fc_version_tmp)
    message(ERROR "Unable to extract version from '${fc_version_str}'")
  endif ()
  # message(STATUS "fc_version_tmp = '${fc_version_tmp}'")
  # message(STATUS "fc_version_tmp = '${fc_version_tmp}'")
  string(REPLACE "(GCC) " "" fc_version_tmp "${fc_version_tmp}")
  string(STRIP ${fc_version_tmp} fc_version_tmp)
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL Intel)
  if (WIN32)
    exec_program(${CMAKE_Fortran_COMPILER}
      OUTPUT_VARIABLE fc_version_str
    )
    string(REGEX MATCH
      "Version [0-9][0-9]\\.[0-9]\\.[0-9]\\.[0-9][0-9][0-9]"
      fc_version_tmp
      ${fc_version_str}
    )
    string(REPLACE "Version " "" fc_version_tmp ${fc_version_tmp})
  else ()
    execute_process(
      COMMAND ${CMAKE_Fortran_COMPILER} --version
      OUTPUT_VARIABLE fc_version_str
    )
    message(STATUS "fc_version_tmp = '${fc_version_str}'")
    # Works on verus: '(ICC) m.n.r' no trailing space on stix
    # string(REGEX MATCH '(GCC) [0-9]+\\.[0-9]+\\.[0-9]+'

##
# Roopa: Check if its ICC or IFORT
##
    string(FIND ${fc_version_str} "(ICC)" fc_icc_tmp)
    string(FIND ${fc_version_str} "(IFORT)" fc_ifort_tmp)

    if (${fc_icc_tmp} STRGREATER "0")
      if (DEBUG_CMAKE)
        message(STATUS "Using Intel's icc compiler")
      endif ()
      set(fc_type_tmp "ICC")
    elseif (${fc_ifort_tmp} STRGREATER "0")
      if (DEBUG_CMAKE)
        message(STATUS "Using Intel's ifort compiler")
      endif ()
      set(fc_type_tmp "IFORT")
    endif ()

    string(REGEX MATCH ".${fc_type_tmp}. [0-9]+\\.[0-9]+\\.[0-9]+"
      fc_version_tmp "${fc_version_str}"
    )
    if (NOT fc_version_tmp)
      message(ERROR "Unable to extract version from '${fc_version_str}'")
    endif ()
    string(REPLACE "(${fc_type_tmp}) " "" fc_version_tmp "${fc_version_tmp}")
    string(STRIP ${fc_version_tmp} fc_version_tmp)
  endif ()
elseif (CMAKE_Fortran_COMPILER MATCHES "icl")
  exec_program(${CMAKE_Fortran_COMPILER}
    OUTPUT_VARIABLE fc_version_tmp
  )
  string(REGEX MATCH
    "w_cproc_p_[0-9][0-9]\\.[0-9]\\.[0-9][0-9][0-9]"
    fc_version_tmp
    ${fc_version_tmp}
  )
  string(REPLACE "w_cproc_p_" "" fc_version_tmp ${fc_version_tmp})
elseif (CMAKE_Fortran_COMPILER MATCHES "cl")
  exec_program(${CMAKE_Fortran_COMPILER}
    OUTPUT_VARIABLE fc_version_tmp
  )
  string(REGEX MATCH
    "Version [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+ for"
    fc_version_tmp
    ${fc_version_tmp}
  )
  string(REPLACE "Version " "" fc_version_tmp ${fc_version_tmp})
  string(REPLACE " for" "" fc_version_tmp ${fc_version_tmp})
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL PathScale)
  exec_program(${CMAKE_Fortran_COMPILER}
    ARGS --version
    OUTPUT_VARIABLE fc_version_tmp
  )
  # ARGS -v # This used to work above?
  # MESSAGE("fc_version_tmp = ${fc_version_tmp}.")
  string(REGEX MATCH
    "Version [0-9]+\\.[0-9]"
    fc_version_tmp
    ${fc_version_tmp}
  )
  string(REPLACE "Version " "" fc_version_tmp ${fc_version_tmp})
  # MESSAGE("fc_version_tmp = ${fc_version_tmp}.")
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL PGI)
  exec_program(${CMAKE_Fortran_COMPILER}
    ARGS -V
    OUTPUT_VARIABLE fc_version_tmp
  )
  string(REGEX MATCH
    "pgf90 [0-9]+\\.[0-9]+-[0-9]+"
    fc_version_tmp
    ${fc_version_tmp}
  )
  # MESSAGE("fc_version_tmp = ${fc_version_tmp}.")
  string(REPLACE "pgf90 " "" fc_version_tmp ${fc_version_tmp})
  # MESSAGE("fc_version_tmp = ${fc_version_tmp}.")
elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL XL)
  exec_program(${CMAKE_Fortran_COMPILER}
    ARGS -qversion
    OUTPUT_VARIABLE fc_version_tmp
  )
  # MESSAGE("fc_version_tmp = ${fc_version_tmp}.")
  string(REGEX MATCH
    "Version: .*"
    fc_version_tmp
    ${fc_version_tmp}
  )
  # MESSAGE("fc_version_tmp = ${fc_version_tmp}.")
  string(REPLACE "Version: " "" fc_version_tmp ${fc_version_tmp})
  # MESSAGE("fc_version_tmp = ${fc_version_tmp}.")
# This does not work, as it sets Boost_INCLUDE_DIR
else ()
  message(FATAL_ERROR "Unknown compiler ID, ${CMAKE_Fortran_COMPILER_ID}.")
endif ()

set(Fortran_VERSION ${fc_version_tmp})
sciPrintString("Fortran_VERSION = '${Fortran_VERSION}'")

