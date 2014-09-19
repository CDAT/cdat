######################################################################
#
# sciEngFindVersion: Function to extract the version and revision
# number of a given executable from the output of "executable --version".
#
# $Id: sciEngFindVersion.cmake 1246 2012-01-31 21:44:54Z dws $
#
# Copyright &copy; 2011-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

# sciEngFindVersion
#
# Args:
#
#   INPUT
#   EXECUTABLE: the full path and name of executable
#   OUTPUT
#   EXE_VERSION: the version and revision number of executable
#
#   ASSUMPTIONS
#   Assumes the output of "executable --version" first
#   prints the executable name, a space, and then the version and revision number.
#   "(executable name) (version number) (revision number)"
#   (e.g. VORPAL 5.1.0 r18555 => 5.1.0-r18555)
#
function(sciEngFindVersion EXECUTABLE_TORUN EXE_VERSION)

# The result of executable --version is sent to standard error
  execute_process(COMMAND ${EXECUTABLE_TORUN} "--version"
    TIMEOUT 60
    ERROR_VARIABLE EXE_STD_ERR
    RESULT_VARIABLE EXE_RSLT
    ERROR_STRIP_TRAILING_WHITESPACE
  )

  if (DEBUG_CMAKE)
    message(STATUS "Result of running ${EXECUTABLE_TORUN} --version = \"${EXE_STD_ERR}\"")
  endif ()

  if (EXE_RSLT)
    message(STATUS "[sciEngFindVersion] Exit status from ${EXECUTABLE_TORUN} --version nonzero; unable to determine version")
    set(EXE_VERSION "VersionNumNotFound" PARENT_SCOPE)
  else ()
    string(LENGTH "${EXE_STD_ERR}" outputlength)
    if (${outputlength})
# string(REPLACE takes the OUTPUT variable first,
# while string(STRIP and (TOLOWER takes the INPUT variable first.
      string(STRIP ${EXE_STD_ERR} EXE_VERSION)
      string(REGEX REPLACE "^([^ ]+[ ]+)(.*)" "\\2" EXE_VERSION ${EXE_VERSION})
      string(REGEX REPLACE " " "-" EXE_VERSION ${EXE_VERSION})
      string(TOLOWER ${EXE_VERSION} EXE_VERSION)
      set(EXE_VERSION ${EXE_VERSION} PARENT_SCOPE)
      message(STATUS "[sciEngFindVersion] Version number to be used: ${EXE_VERSION}")
    else ()
      message(STATUS "[sciEngFindVersion] Empty ${EXECUTABLE_TORUN} --version output, unable to determine version")
      set(EXE_VERSION "VersionNumNotFound" PARENT_SCOPE)
    endif ()
  endif ()
endfunction()
