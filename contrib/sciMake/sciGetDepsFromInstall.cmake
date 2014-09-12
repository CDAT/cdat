######################################################################
#
# sciGetDepsFromInstall: From the installation of one project, find
#   the installations of other other projects from config.summary.
#
# $Id: sciGetDepsFromInstall.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

include(${TXCMAKE_DIR}/sciSeparateLibs.cmake)

# sciGetDepsFromInstall
#
# Args:
#  name:     cmake name of the dependency to find variables of
#  founddir: directory of the found installation, under which one finds
#              config.summary
#  atname:   autotools name of the dependency to find variables of
#
function(sciGetDepsFromInstall cmname founddir atname)

  if (EXISTS ${founddir}/share/config.summary)
    set(configfile ${founddir}/share/config.summary)
  else ()
    message(STATUS "${founddir}/share/config.summary does not exist!")
    if (ENABLE_PARALLEL)
      set(configfilecand ${founddir}/share-par/config.summary)
    else ()
      set(configfilecand ${founddir}/share-ser/config.summary)
    endif ()
    if (EXISTS ${configfilecand})
      set(configfile ${configfilecand})
    else ()
      message(STATUS "${configfilecand} does not exist!")
      return()
    endif ()
  endif ()
  if (DEBUG_CMAKE)
    message(STATUS "configfile = ${configfile}")
  endif ()

# Look for a cmake name in the file
  foreach (sfx DIR EXECUTABLES FILES INCLUDE_DIRS MODULE_DIRS LIBFLAGS LIBRARY_DIRS LIBRARY_NAMES LIBRARIES STLIBS)
    file(STRINGS ${configfile} ${cmname}_${sfx} REGEX ${cmname}_${sfx})
    # message("${cmname}_${sfx} = ${${cmname}_${sfx}}")
    string(REGEX REPLACE "^.*= *" "" ${cmname}_${sfx} "${${cmname}_${sfx}}")
    # message("${cmname}_${sfx} = ${${cmname}_${sfx}}")
# This comes with \;, so must remove \
    string(REGEX REPLACE "\\\\" "" ${cmname}_${sfx} "${${cmname}_${sfx}}")
    # message("${cmname}_${sfx} = ${${cmname}_${sfx}}")
  endforeach ()

# For each empty variable, try to resolve using the autotools name
# Includes
  if (NOT ${cmname}_INCLUDE_DIRS)
    file(STRINGS ${configfile} ${atname}_INCDIR REGEX ${atname}_INCDIR)
    string(REGEX REPLACE "^.*: *" "" ${atname}_INCDIR "${${atname}_INCDIR}")
    string(REPLACE " " ";" ${cmname}_INCLUDE_DIRS "${${atname}_INCDIR}")
  endif ()
  if (${cmname}_INCLUDE_DIRS)
    set(HAVE_${cmname} 1 CACHE BOOL "Whether the ${cmname} package found.")
    set(HAVE_${atname} 1 CACHE BOOL "Whether the ${cmname} package found.")
  endif ()
# Modules
  if (NOT ${cmname}_MODULE_DIRS)
    file(STRINGS ${configfile} ${atname}_MODDIR REGEX ${atname}_MODDIR)
    string(REGEX REPLACE "^.*: *" "" ${atname}_MODDIR "${${atname}_MODDIR}")
    string(REPLACE " " ";" ${cmname}_MODULE_DIRS "${${atname}_MODDIR}")
  endif ()
  if (${cmname}_MODULE_DIRS)
    set(HAVE_${cmname} 1 CACHE BOOL "Whether the ${cmname} package found.")
    set(HAVE_${atname} 1 CACHE BOOL "Whether the ${cmname} package found.")
  endif ()
# Libraries
  if (NOT ${cmname}_LIBRARIES)
    file(STRINGS ${configfile} ${atname}_LIBS REGEX ${atname}_LIBS)
    string(REGEX REPLACE "^.*: *" "" ${atname}_LIBS "${${atname}_LIBS}")
    sciMakeLibList(${atname}_LIBS)
# Separate out the different cmake variables
    sciSeparateLibs("${${atname}_LIBS}" ${cmname}_LIBFLAGS ${cmname}_LIBRARIES ${cmname}_LIBRARY_DIRS ${cmname}_LIBRARY_NAMES ${cmname}_FRAMEWORKS)
  endif ()
  if (${cmname}_LIBRARIES)
    set(HAVE_${cmname} 1 CACHE BOOL "Whether the ${cmname} package found.")
    set(HAVE_${atname} 1 CACHE BOOL "Whether the ${cmname} package found.")
  endif ()
  if (NOT ${cmname}_STLIBS)
    file(STRINGS ${configfile} ${atname}_ALIBS REGEX ${atname}_ALIBS)
    string(REGEX REPLACE "^.*: *" "" ${atname}_ALIBS "${${atname}_ALIBS}")
    string(REPLACE " " ";" ${cmname}_STLIBS "${${atname}_ALIBS}")
  endif ()
  if (${cmname}_LIBRARIES OR ${cmname}_INCLUDE_DIRS)
# Assume finding either works
    set(HAVE_${cmname} 1 CACHE BOOL "Whether the ${cmname} package found.")
    set(HAVE_${atname} 1 CACHE BOOL "Whether the ${atname} package found.")
    set(${cmname}_FOUND TRUE PARENT_SCOPE)
    set(${atname}_FOUND TRUE PARENT_SCOPE)
  else ()
    set(HAVE_${cmname} 0 CACHE BOOL "Whether the ${cmname} package found.")
    set(HAVE_${atname} 0 CACHE BOOL "Whether the ${atname} package found.")
    set(${cmname}_FOUND FALSE PARENT_SCOPE)
    set(${atname}_FOUND FALSE PARENT_SCOPE)
  endif ()

# Get all vars to parent scope
  foreach (varpart DIR EXECUTABLES FILES INCLUDE_DIRS MODULE_DIRS LIBFLAGS LIBRARY_DIRS LIBRARY_NAMES LIBRARIES STLIBS)
    set(${cmname}_${varpart} "${${cmname}_${varpart}}" PARENT_SCOPE)
  endforeach ()
  sciPrintCMakeResults(${cmname})

# Print atname variables
  foreach (varpart INCDIR MODDIR LIBS ALIBS)
    set(${atname}_${varpart} "${${atname}_${varpart}}" PARENT_SCOPE)
  endforeach ()
  sciPrintAutotoolsResults(${atname})

endfunction()

