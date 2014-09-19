######################################################################
#
# sciSeparateLibs: Separate unix style libs into cmake flags and lib lists
#
# $Id: sciSeparateLibs.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

# sciClusterLibs
#   From space delimited to a regular list
macro(sciMakeLibList libsvar)
  string(STRIP "${${libsvar}}" ${libsvar})
  string(REGEX REPLACE "  *" " " ${libsvar} "${${libsvar}}")
  string(REGEX REPLACE " -L  *" " -L" ${libsvar} "${${libsvar}}")
  string(REGEX REPLACE " " ";" ${libsvar} "${${libsvar}}")
endmacro()

# sciSeparateLibs
#
# Args:
#  origlibs:   libraries as a list
#  flagsvar:   name of flags variable
#  libsvar:    name of libs variable
#  libdirsvar: name of libdirs variable
#  libnamesvar:   name of libs variable
#  frmwrksvar: name of frameworks variable
#
function(sciSeparateLibs
    origlibs flagsvar libsvar libdirsvar libnamesvar frmwrksvar)

# Handle empty case
  if (NOT origlibs)
    set(${flagsvar} "" PARENT_SCOPE)
    set(${libsvar} "" PARENT_SCOPE)
    set(${libdirsvar} "" PARENT_SCOPE)
    set(${libnamesvar} "" PARENT_SCOPE)
    set(${frmwrksvar} "" PARENT_SCOPE)
    return()
  endif ()

# Separate out the libs
# Assume a proper list
  # string(STRIP "${origlibs}" origlibs)
  # string(REPLACE " " ";" origlibs "${origlibs}")
  if (DEBUG_CMAKE)
    message(STATUS "Separating ${origlibs}.")
  endif ()
  set(lastflag)
  foreach (txlib ${origlibs})
    if (lastflag)
      if (lastflag STREQUAL framework)
        set(${frmwrksvar} ${${frmwrksvar}} "${txlib}")
      endif ()
      set(lastflag)
    elseif (${txlib} STREQUAL "-framework")
      set(lastflag "framework")
    elseif (${txlib} MATCHES "^-l")
      string(REGEX REPLACE "^-l" "" txlib ${txlib})
      set(${libnamesvar} ${${libnamesvar}} ${txlib})
    elseif (${txlib} MATCHES "^-L")
      string(REGEX REPLACE "^-L" "" txlib ${txlib})
      set(${libdirsvar} ${${libdirsvar}} ${txlib})
    elseif (${txlib} MATCHES "^-Wl,-rpath")
# Ignore rpath flags
    else ()
      set(${flagsvar} ${${flagsvar}} ${txlib})
    endif ()
  endforeach ()
  if (${flagsvar})
    list(REMOVE_DUPLICATES ${flagsvar})

# Flags have to be strings
    string(REPLACE ";" " " ${flagsvar} "${${flagsvar}}")
  endif ()
  if (DEBUG_CMAKE)
    message(STATUS "${flagsvar} = ${${flagsvar}}.")
  endif ()

# Remove earlier occurences of duplications
  foreach (j libnamesvar libdirsvar frmwrksvar)
    if (${${j}})
      list(REVERSE ${${j}})
      list(REMOVE_DUPLICATES ${${j}})
      list(REVERSE ${${j}})
    endif ()
    if (DEBUG_CMAKE)
      message(STATUS "${${j}} = ${${${j}}}.")
    endif ()
  endforeach ()

# Find each library
  foreach (name ${${libnamesvar}})
    unset(${name}_LIBRARY)
    if (DEBUG_CMAKE)
      message("Looking for ${name} in ${${libdirsvar}}.")
    endif ()
    find_library(${name}_LIBRARY ${name} ${${libdirsvar}} NO_DEFAULT_PATH)
    if (${name}_LIBRARY)
      if (DEBUG_CMAKE)
        message("Found: ${${name}_LIBRARY}.")
      endif ()
      list(APPEND ${libsvar} ${${name}_LIBRARY})
    else ()
      if (DEBUG_CMAKE)
        message("${name}_LIBRARY not found.")
      endif ()
    endif ()
  endforeach ()

# Put everything in parent scope
  set(${flagsvar} "${${flagsvar}}" PARENT_SCOPE)
  set(${libsvar} "${${libsvar}}" PARENT_SCOPE)
  set(${libdirsvar} "${${libdirsvar}}" PARENT_SCOPE)
  set(${libnamesvar} "${${libnamesvar}}" PARENT_SCOPE)
  set(${frmwrksvar} "${${frmwrksvar}}" PARENT_SCOPE)

endfunction()

