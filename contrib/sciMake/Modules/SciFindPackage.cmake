######################################################################
#
# SciFindPackage: find includes and libraries of a package
#
# $Id: SciFindPackage.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################
#
# Variables which can be used to change the behavior of this script
#
# Where "txpkgreg" is the regularized package name
#(package name with all "." and "-" replaced with "_")
# Where "txpkguc" is the UPPERCASE regularized package name
#
#  DEBUG_CMAKE - if true, outputs verbose debugging information
#(default false)
#
#  ENABLE_${txpkguc} - if false, will not search for package
#(default true)
#
#  ${txpkgreg}_FIND_QUIETLY - if true, will succeed silently
#(default - not defined, which is treated as false)
#
#  ${txpkgreg}_FIND_REQUIRED - if true, will issue a fatal error if
#    package not found
#(default - not defined, which is treated as false)
#
#  ${txpkguc}_DIR - a search directory hint
#
#  SUPRA_SEARCH_PATH - used to specify various top-level search directories
#
######################################################################
#
#  Variables which will be defined by this script
#  In the event that a variable has no valid value, it will be set to
#   "${varname}-NOTFOUND"
#
#  NOTE: Some variables use txpkguc, and some use txpkgreg.
#    Caveat emptor.
#
#    ${txpkguc}_FOUND - true if package found
#
#  EXECUTABLES:
#    ${txpkgreg}_EXECUTABLES - list of found executables, including full
#      path to each.  Only defined for specifically requested executables.
#    ${txpkgreg}_yyy - full name & path to executable "yyy"
#      Only defined for specifically requested executables.
#
#  FILES:
#    ${txpkgreg}_FILES - list of found files, including a full path to each
#    ${txpkgreg}_yyy - full name and path to file "yyy"
#
#  MODULES:
#    ${txpkgreg}_MODULE_DIRS - a list of all module directories found
#    ${txpkgreg}_yyy_MOD - full path to individual module yyy.{mod,MOD}
#      Only defined for specifically requested modules
#
#  HEADERS:
#    ${txpkgreg}_INCLUDE_DIRS - a list of all include directories found
#    ${txpkgreg}_yyy - full path to individual header yyy
#      Only defined for specifically requested headers
#
#  LIBRARIES:
#    ${txpkgreg}_{library_name}_LIBRARY - full path to the individual
#      $library_name library. Only defined for specifically requested libraries.
#    ${txpkgreg}_LIBRARIES - list of found libraries, including full path to
#      each.  Only defined for specifically requested libraries.
#   ${txpkgreg}_STLIBS - list of all found static libraries.
#      Only defined for libraries existing in ${txpkgreg}_LIBRARIES
#   ${txpkgreg}_DLLS - windows only, list of all found dlls
#      Only defined for libraries existing in ${txpkgreg}_LIBRARIES
#
#######################################################################

# SciGetStaticLibs
#
# Given a list of libraries, create a new list, where for any
# dynamic library with a static library next to it, the new list
# contains the static library.  Otherwise it contains the original
# dynamic library.
#
# Args:
# origlibs: the original libraries
# statlibsvar: the variable holding the static libs when found
#
FUNCTION(SciGetStaticLibs origlibs statlibsvar)
  # set(origlibs ${${origlibsvar}})
  set(statlibs)
  if (DEBUG_CMAKE)
    message(STATUS "[SciFindPackage]: original libs = ${origlibs}.")
    message(STATUS "[SciFindPackage]: statlibsvar = ${statlibsvar}.")
  endif ()
  foreach (lib ${origlibs})
    set(newlib ${lib})
    set(havestlib FALSE)
    if (${lib} MATCHES "\\.a$" OR ${lib} MATCHES "\\.lib$")
      if (DEBUG_CMAKE)
        message(STATUS "${lib} is a static library.")
      endif ()
      set(havestlib TRUE)
    endif ()
# If not static, try replacing suffix
    if (NOT ${havestlib})
      foreach (sfx so;dylib;dll)
        if (${lib} MATCHES "\\.${sfx}$")
          if (DEBUG_CMAKE)
            message(STATUS "${lib} is a shared library.")
          endif ()
          get_filename_component(libdir ${lib}/.. REALPATH)
# NAME_WE takes from first .  We generally need from last
          get_filename_component(libname "${lib}" NAME)
          string(REGEX REPLACE "\\.[^\\.]*$" "" libname "${libname}")
          if (${sfx} STREQUAL "dll")
            set(newsfx "lib")
          else ()
            set(newsfx "a")
          endif ()
          if (EXISTS ${libdir}/${libname}.${newsfx})
            set(newlib ${libdir}/${libname}.${newsfx})
            set(havestlib TRUE)
          endif ()
          break()
        endif ()
      endforeach ()
    endif ()
# If still do not have static, try pulling out of library flags
    if (${havestlib})
      list(APPEND statlibs ${newlib})
    elseif (${lib} MATCHES "^-L")
      if (DEBUG_CMAKE)
        message(STATUS "${lib} defined by flags.")
      endif ()
      # set(libargs ${lib}) # Create a list
      string(REPLACE " " ";" libargs "${lib}")
      if (DEBUG_CMAKE)
        message(STATUS "libargs = ${libargs}.")
      endif ()
      set(libdirargs)
      set(libdirs)
      set(libnames)
      foreach (libarg ${libargs})
        if (DEBUG_CMAKE)
          message(STATUS "libarg = ${libarg}.")
        endif ()
        if (${libarg} MATCHES "^-L")
          list(APPEND libdirargs ${libarg})
          string(REGEX REPLACE "^-L" "" libdir ${libarg})
          list(APPEND libdirs ${libdir})
        elseif (${libarg} MATCHES "^-l")
          string(REGEX REPLACE "^-l" "" libname ${libarg})
          list(APPEND libnames ${libname})
        endif ()
      endforeach ()
      if (DEBUG_CMAKE)
        message(STATUS "libdirs = ${libdirs}.")
        message(STATUS "libnames = ${libnames}.")
      endif ()
      set(stlibs)
      foreach (libname ${libnames})
        set(lib)
        foreach (libdir ${libdirs})
          if (EXISTS ${libdir}/lib${libname}.a)
            set(lib ${libdir}/lib${libname}.a)
            break()
          elseif (EXISTS ${libdir}/${libname}.lib)
            set(lib ${libdir}/${libname}.lib)
            break()
          endif ()
        endforeach ()
        if (lib)
          list(APPEND stlibs ${lib})
        else ()
          list(APPEND stlibs "${libdirargs} -l${libname}")
        endif ()
      endforeach ()
      list(APPEND statlibs ${stlibs})
    else ()
# If still not static, look in system dirs
      foreach (libdir /usr/lib64;/usr/lib)
        if (EXISTS ${libdir}/lib${lib}.a)
          set(newlib ${libdir}/lib${lib}.a)
          set(havestlib TRUE)
          break()
        endif ()
      endforeach ()
      list(APPEND statlibs ${newlib})
    endif ()
  endforeach ()
  if (DEBUG_CMAKE)
    message(STATUS "[SciFindPackage]: static libs = ${statlibs}.")
  endif ()
  set(${statlibsvar} ${statlibs} PARENT_SCOPE)
ENDFUNCTION()

# SciGetRealDir
#
# Given a directory name, find the real directory name after
#  first trying to resolve it as a shortcut if on Windows,
#  then, if that does not work, trying to resolve it as a soft
#  link.
#
# Args:
#  canddir: the variable holding the directory candidate
#  realdirvar: the variable holding the directory after following any shortcuts
#
FUNCTION(SciGetRealDir canddir realdirvar)
  # MESSAGE("realdirvar = ${realdirvar}.")
  set(${realdirvar})
  if (canddir)
    if (DEBUG_CMAKE)
      message(STATUS "Finding the directory for ${canddir}.")
    endif ()
# Follow an existing Windows shortcut
    if (WIN32)
      if (EXISTS ${canddir}.lnk)
        exec_program(readshortcut ARGS ${canddir}.lnk
          OUTPUT_VARIABLE idir
          RETURN_VALUE ires)
        if (ires)
          message(FATAL_ERROR "readshortcut error on ${canddir}.lnk.")
        endif ()
        exec_program(cygpath ARGS -am ${idir} OUTPUT_VARIABLE rd)
      else ()
        set(rd ${canddir})
      endif ()
    else ()
       get_filename_component(rd ${canddir} REALPATH)
    endif ()
  endif ()
  set(${realdirvar} ${rd} PARENT_SCOPE)
ENDFUNCTION()

#
# SciFindPackage
#
# Args:
# PACKAGE: the prefix for the defined variables
# INSTALL_DIRS: the names for the installation subdirectory.  Defaults
#   to lower cased txpkgname
# EXECUTABLES: the executables to look for
# HEADERS: the header files to look for.
# LIBRARIES: the libraries
# MODULES: Fortran module files
# EXECUTABLE_SUBDIRS: executable subdirs
# INCLUDE_SUBDIRS: include subdirectories
# LIBRARY_SUBDIRS: library subdirectories
#
# NOTE: One cannot reset calling variables
# NOTE: lists should be delimited by semicolons.
#(which is the default format used by CMake)
#
include(CMakeParseArguments)
macro(SciFindPackage)
  CMAKE_PARSE_ARGUMENTS(TFP
    ""
    "PACKAGE;INSTALL_DIR"
    "INSTALL_DIRS;EXECUTABLES;HEADERS;LIBRARIES;FILES;MODULES;EXECUTABLE_SUBDIRS;INCLUDE_SUBDIRS;MODULE_SUBDIRS;LIBRARY_SUBDIRS;FILE_SUBDIRS"
    ${ARGN}
  )

  # This message is purposefully NOT a STATUS message
  # To provide more readable output
  message("")
  message("--------- SciFindPackage looking for ${TFP_PACKAGE} ---------")

  if (DEBUG_CMAKE)
    message(STATUS "Using verbose options")
    message(STATUS "SciFindPackage called with arguments:
    PACKAGE = ${TFP_PACKAGE}
    INSTALL_DIR = ${TFP_INSTALL_DIR}
    INSTALL_DIRS = ${TFP_INSTALL_DIRS}
    EXECUTABLES = ${TFP_EXECUTABLES}
    HEADERS = ${TFP_HEADERS}
    LIBRARIES = ${TFP_LIBRARIES}
    MODULES = ${TFP_MODULES}
    EXECUTABLE_SUBDIRS = ${TFP_EXECUTABLE_SUBDIRS}
    INCLUDE_SUBDIRS = ${TFP_INCLUDE_SUBDIRS}
    LIBRARY_SUBDIRS = ${TFP_LIBRARY_SUBDIRS}")
  endif ()

# Construct various names(upper/lower case) for package
  string(REGEX REPLACE "[./-]" "_" txpkgreg ${TFP_PACKAGE})
# txpkgreg is the regularized package name
  string(TOUPPER ${txpkgreg} txpkguc)
  if (DEBUG_CMAKE)
    message(STATUS "ENABLE_${txpkguc} = ${ENABLE_${txpkguc}}")
  endif ()
  if (NOT DEFINED ENABLE_${txpkguc})
    set(ENABLE_${txpkguc} TRUE)
  endif ()
  if (NOT ENABLE_${txpkguc})
    message(STATUS "Disabling ${txpkgreg}.")
    set(${txpkguc}_FOUND FALSE)
    if (DEBUG_CMAKE)
      message(STATUS "${txpkguc}_FOUND set to FALSE.")
    endif ()
    RETURN()
  endif ()
  string(TOLOWER ${txpkgreg} txpkglc)
  set(txpkginst ${TFP_INSTALL_DIR} ${TFP_INSTALL_DIRS})
  if (NOT txpkginst)
    set(txpkginst ${txpkginst} ${txpkglc})
  endif ()
  if (DEBUG_CMAKE)
    message(STATUS "txpkginst = ${txpkginst}.")
  endif ()

# Find the possible directories and put them into path
# Order: command-line define, environment variable, supra-search-path subdirs,
# supra-search-path dirs
  if (DEBUG_CMAKE)
    message("SciFindPackage] SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH}")
  endif ()
  set(txpath)
# Command-line define
  set(txdir ${${txpkguc}_DIR})
  if (txdir)
    SciGetRealDir(${txdir} txpath)
  endif ()
# Environment variable
  set(idir $ENV{${txpkguc}_DIR})
  if (idir)
    SciGetRealDir(${idir} txdir)
    set(txpath ${txpath} ${txdir})
  endif ()
# Supra-search-path dirs
  foreach (instdir ${txpkginst})
    foreach (spdir ${SUPRA_SEARCH_PATH})
      set(idir ${spdir}/${instdir})
      if (EXISTS ${idir})
        SciGetRealDir(${idir} txdir)
        set(txpath ${txpath} ${txdir})
      endif ()
    endforeach (spdir ${SUPRA_SEARCH_PATH})
  endforeach ()
# Supra-search-path dirs
  foreach (spdir ${SUPRA_SEARCH_PATH})
    set(idir ${spdir})
    if (EXISTS ${idir})
      SciGetRealDir(${idir} txdir)
      set(txpath ${txpath} ${txdir})
    endif ()
  endforeach (spdir ${SUPRA_SEARCH_PATH})
# Any found?
  list(LENGTH txpath txpathlen)
  if (DEBUG_CMAKE)
    if (NOT txpathlen)
      message(FATAL_ERROR "txpath is empty.")
    else ()
      message(STATUS "txpath = ${txpath}")
    endif ()
  endif ()

#######################################################################
#
# Look for EXECUTABLES
# Variables defined:
#   XXX_YYY - CACHED
#     Where to find the YYY tool that comes with XXX.
#   XXX_EXECUTABLES - CACHED
#     List of all executables found for package XXX.
#
#######################################################################

# Create the search paths
  string(LENGTH "${TFP_EXECUTABLES}" txexecslen)
  if (${txexecslen})
    string(LENGTH "${TFP_EXECUTABLE_SUBDIRS}" txlen)
    if (${txlen})
      set(txexecsubdirs ${TFP_EXECUTABLE_SUBDIRS})
    else ()
# Here we use the default search directory "bin"
      set(txexecsubdirs bin)
    endif ()

    if (DEBUG_CMAKE)
      message(STATUS "Looking for executables under ${txpath} with subdirs, ${txexecsubdirs}")
    endif ()

# Clear the list of executable dirs
# As we loop over the executables, we will add every new directory to the list
# And afterwards, we'll remove duplicates
    set(${txpkgreg}_EXECUTABLES)
    set(${txpkgreg}_FOUND_SOME_EXECUTABLE FALSE)

# Loop over list of executables and try to find each one
    foreach (txexec ${TFP_EXECUTABLES})

# Create the variable that's specific to this executable
      string(REGEX REPLACE "[./-]" "_" txexecvar ${txexec})
      set(txexecvar ${txpkgreg}_${txexecvar})
      if (DEBUG_CMAKE)
        message(STATUS "Calling FIND_PROGRAM with txexec = ${txexec}")
        message(STATUS "Calling FIND_PROGRAM with paths = ${txpath}")
        message(STATUS "Calling FIND_PROGRAM with path_suffixes ${txexecsubdirs}")
      endif ()

# First look in specified path
      find_program(${txexecvar}
        "${txexec}"
        PATHS ${txpath}
        PATH_SUFFIXES "${txexecsubdirs}"
        NO_DEFAULT_PATH
        DOC "Path to the ${txexec} executable"
      )

# If not found, try again with default paths
      if (NOT ${txexecvar})
        if (DEBUG_CMAKE)
          message(STATUS "Failed to find ${txexec} in search path, trying default paths.")
        endif ()

        find_program(${txexecvar}
          ${txexec}
          PATHS ${txpath}
          PATH_SUFFIXES "${txexecsubdirs}"
          DOC "Path to the ${txexec} executable"
        )
      endif ()

      if (DEBUG_CMAKE)
        message(STATUS "${txexecvar} is ${${txexecvar}}")
      endif ()

      if (${${txexecvar}} MATCHES NOTFOUND)
# The WARNING option will actually give a CMake stack trace
# And I didn't want that
# So instead, use NO option, and start the string with WARNING
        message("WARNING - Unable to locate executable ${txexec}")
      else ()
        set(${txpkgreg}_FOUND_SOME_EXECUTABLE TRUE)
# Add to list of all executables for this package
        set(${txpkgreg}_EXECUTABLES
          ${${txpkgreg}_EXECUTABLES}
          ${${txexecvar}}
        )
      endif ()
    endforeach (txexec ${TFP_EXECUTABLES})

# Clean up the list of all executable directories
    if (${txpkgreg}_FOUND_SOME_EXECUTABLE)
      list(REMOVE_DUPLICATES "${txpkgreg}_EXECUTABLES")
    endif ()

# Print results if in debug mode
    if (DEBUG_CMAKE)
      message(STATUS "List of all executables for ${txpkgreg}:")
      message(STATUS "${txpkgreg}_EXECUTABLES = ${${txpkgreg}_EXECUTABLES}")
    endif ()

# Cache the completed list
    set(${txpkgreg}_EXECUTABLES
      ${${txpkgreg}_EXECUTABLES}
      CACHE STRING "List of all executables for ${txpkgreg}"
    )
  endif ()

#######################################################################
#
# Look for FILES.
# Like EXECUTABLES, but using find_file, instead of find_program
# Variables defined:
#   XXX_YYY - CACHED
#     Where to find the YYY tool that comes with XXX.
#   XXX_FILES - CACHED
#     List of all executables found for package XXX.
#
#######################################################################

# Create the search paths
  string(LENGTH "${TFP_FILES}" txfileslen)
  if (${txfileslen})
    string(LENGTH "${TFP_FILE_SUBDIRS}" txlen)
    if (${txlen})
      set(txfilesubdirs ${TFP_FILE_SUBDIRS})
    else ()
# Here we use the default search directory "bin"
      # set(txfilesubdirs .)
    endif ()

    if (DEBUG_CMAKE)
      message(STATUS "Looking for files under ${txpath} with subdirs, ${txfilesubdirs}")
    endif ()

# Clear the list of file dirs
# As we loop over the files, we will add every new directory to the list
# And afterwards, we'll remove duplicates
    set(${txpkgreg}_FILES)
    set(${txpkgreg}_FOUND_SOME_FILE FALSE)

# Loop over list of files and try to find each one
    foreach (txfile ${TFP_FILES})

# Create the variable that's specific to this file
      string(REGEX REPLACE "[./-]" "_" txfilevar ${txfile})
      set(txfilevar ${txpkgreg}_${txfilevar})
      if (DEBUG_CMAKE)
        message(STATUS "Calling FIND_FILE with txfile = ${txfile}")
        message(STATUS "Calling FIND_FILE with paths = ${txpath}")
        message(STATUS "Calling FIND_FILE with path_suffixes ${txfilesubdirs}")
      endif ()

# First look in specified path
      find_file(${txfilevar}
        "${txfile}"
        PATHS ${txpath}
        PATH_SUFFIXES ${txfilesubdirs}
        NO_DEFAULT_PATH
        DOC "Path to the ${txfile} file"
      )

# If not found, try again with default paths
      if (NOT ${txfilevar})
        if (DEBUG_CMAKE)
          message(STATUS "Failed to find ${txfile} in search path, trying default paths.")
        endif ()

        find_file(${txfilevar}
          ${txfile}
          PATHS ${txpath}
          PATH_SUFFIXES ${txfilesubdirs}
          DOC "Path to the ${txfile} file"
        )
      endif ()

      if (DEBUG_CMAKE)
        message(STATUS "${txfilevar} is ${${txfilevar}}")
      endif ()

      if (${${txfilevar}} MATCHES NOTFOUND)
# The WARNING option will actually give a CMake stack trace
# And I didn't want that
# So instead, use NO option, and start the string with WARNING
        message("WARNING - Unable to locate file ${txfile}")
      else ()
        set(${txpkgreg}_FOUND_SOME_FILE TRUE)
# Add to list of all files for this package
        set(${txpkgreg}_FILES
          ${${txpkgreg}_FILES}
          ${${txfilevar}}
        )
      endif ()
    endforeach (txfile ${TFP_FILES})

# Clean up the list of all file directories
    if (${txpkgreg}_FOUND_SOME_FILE)
      list(REMOVE_DUPLICATES "${txpkgreg}_FILES")
    endif ()

# Print results if in debug mode
    if (DEBUG_CMAKE)
      message(STATUS "List of all files for ${txpkgreg}:")
      message(STATUS "${txpkgreg}_FILES = ${${txpkgreg}_FILES}")
    endif ()

# End of this block
  endif ()

#######################################################################
#
# Look for MODULES
# Like FILES, but append module suffix
# Variables defined:
#   XXX_YYY - CACHED
#     Where to find the YYY tool that comes with XXX.
#   XXX_MODULES - CACHED
#     List of all executables found for package XXX.
#
#######################################################################

# Create the search paths
  string(LENGTH "${TFP_MODULES}" txmoduleslen)
  if (${txmoduleslen} AND NOT TX_FC_MODULE_SUFFIX)
    message(WARNING "[SciFindPackage.cmake] Cannot find Fortran module files, as TX_FC_MODULE_SUFFIX is not defined with a call to SciFortranChecks.cmake.")
  elseif (${txmoduleslen})
    string(LENGTH "${TFP_MODULE_SUBDIRS}" txlen)
    if (${txlen})
      set(txmodulesubdirs ${TFP_MODULE_SUBDIRS})
    else ()
# Default subdirectory
      set(txmodulesubdirs include lib)
    endif ()

    if (DEBUG_CMAKE)
      message(STATUS "Looking for modules under ${txpath} with subdirs, ${txmodulesubdirs}")
    endif ()

# Clear the list of file dirs
# As we loop over the files, we will add every new directory to the list
# And afterwards, we'll remove duplicates
    set(${txpkgreg}_MODULES)
    set(${txpkgreg}_MODULE_DIRS)
    set(${txpkgreg}_FOUND_SOME_MODULE FALSE)

# Loop over list of files and try to find each one
    foreach (txmodule ${TFP_MODULES})

# Capitalize as needed
      if (TX_FC_MODULENAME_CAPITALIZED)
        string(TOUPPER ${txmodule} txmodfile)
      else ()
        set(txmodfile ${txmodule})
      endif ()
      set(txmodfile "${txmodfile}.${TX_FC_MODULE_SUFFIX}")

# Create the variable that's specific to this file
      string(REGEX REPLACE "[./-]" "_" txmodulevar ${txmodule})
      set(txmodulevar ${txpkgreg}_${txmodulevar}_MOD)
      if (DEBUG_CMAKE)
        message(STATUS "Calling FIND_FILE with txmodfile = ${txmodfile}")
        message(STATUS "Calling FIND_FILE with paths = ${txpath}")
        message(STATUS "Calling FIND_FILE with path_suffixes ${txmodulesubdirs}")
      endif ()

# First look in specified path
      find_file(${txmodulevar}
        "${txmodfile}"
        PATHS ${txpath}
        PATH_SUFFIXES ${txmodulesubdirs}
        NO_DEFAULT_PATH
        DOC "Path to the ${txmodfile} file"
      )

# If not found, try again with default paths
      if (NOT ${txmodulevar})
        if (DEBUG_CMAKE)
          message(STATUS "Failed to find ${txmodfile} in search path, trying default paths.")
        endif ()

        find_file(${txmodulevar}
          ${txmodfile}
          PATHS ${txpath}
          PATH_SUFFIXES ${txmodulesubdirs}
          DOC "Path to the ${txmodfile} file"
        )
      endif ()

      if (DEBUG_CMAKE)
        message(STATUS "${txmodulevar} is ${${txmodulevar}}")
      endif ()

      if (${${txmodulevar}} MATCHES NOTFOUND)
# The WARNING option will actually give a CMake stack trace
# And I didn't want that
# So instead, use NO option, and start the string with WARNING
        message("WARNING - Unable to locate file ${txmodule}")
      else ()
        set(${txpkgreg}_FOUND_SOME_MODULE TRUE)
# Add to list of all files for this package
        set(${txpkgreg}_MODULES
          ${${txpkgreg}_MODULES}
          ${${txmodulevar}}
        )
        set(${txmodulevar} ${${txmodulevar}}
          CACHE FILEPATH "${txmodule} file."
        )
        get_filename_component(dir ${${txmodulevar}}/.. REALPATH)
        set(${txpkgreg}_MODULE_DIRS ${${txpkgreg}_MODULE_DIRS} ${dir})
      endif ()
    endforeach (txmodule ${TFP_MODULES})

# Clean up the list of all module directories
    if (${txpkgreg}_FOUND_SOME_MODULE)
      list(REMOVE_DUPLICATES "${txpkgreg}_MODULES")
      list(REMOVE_DUPLICATES "${txpkgreg}_MODULE_DIRS")
    endif ()

# Print results if in debug mode
    if (DEBUG_CMAKE)
      message(STATUS "List of all modules for ${txpkgreg}:")
      message(STATUS "${txpkgreg}_MODULES = ${${txpkgreg}_MODULES}")
    endif ()

# End of this block
  endif ()

##########################################################################
# Look for HEADERS
# Finding none is okay(e.g. zlib has no headers)
# Will set:
#   XXX_INCLUDE_DIRS - NOT CACHED
#     The final set of include directories listed in one variable.
#   XXX_yy_h - NOT CACHED
#     Where to find xxx_yy.h
##########################################################################

  string(LENGTH "${TFP_HEADERS}" txhdrslen)
  if (${txhdrslen})
# Find include subdirs
    string(LENGTH "${TFP_INCLUDE_SUBDIRS}" txlen)
    if (${txlen})
      set(txincsubdirs ${TFP_INCLUDE_SUBDIRS})
    else ()
      set(txincsubdirs include)
    endif ()

    if (DEBUG_CMAKE)
      message(STATUS "Looking for headers under ${txpath} with subdirs, ${txincsubdirs}")
    endif ()

# Clear the variables
    set(${txpkgreg}_INCLUDE_DIRS)
    set(${txpkgreg}_FOUND_SOME_HEADER FALSE)

# Look for each header in turn
    foreach (txhdr ${TFP_HEADERS})

# Create variable name from cleaned name
      string(REGEX REPLACE "[./-]" "_" txhdrvar ${txhdr})
      set(txhdrvar ${txpkgreg}_${txhdrvar})
      set(txhdrdirvar ${txhdrvar}_INCLUDE_DIR)

# First look in specified path
      find_path(${txhdrdirvar}
        ${txhdr}
        PATHS ${txpath}
        PATH_SUFFIXES ${txincsubdirs}
        NO_DEFAULT_PATH)

# If not found, also look in default paths
      if (NOT ${txhdrdirvar})
        find_path(${txhdrdirvar}
          ${txhdr}
          PATHS ${txpath}
          PATH_SUFFIXES ${txincsubdirs})
      endif ()

# Found or not?
      if (${txhdrdirvar})
# Create header variable and push into cache
        set(${txhdrdirvar} ${${txhdrdirvar}}
          CACHE FILEPATH "Directory containing ${txhdr}."
        )
        if (DEBUG_CMAKE)
           message(STATUS "${txhdrdirvar} = ${${txhdrdirvar}}.")
        endif ()
        set(${txpkgreg}_FOUND_SOME_HEADER TRUE)
        set(${txhdrvar} ${${txhdrdirvar}}/${txhdr}
          CACHE FILEPATH "${txhdr} file."
        )
# Add the directory of this header to the list of all include directories
        set(${txpkgreg}_INCLUDE_DIRS ${${txpkgreg}_INCLUDE_DIRS}
          ${${txhdrdirvar}}
        )
      else ()
        message("WARNING - Unable to find header: ${txhdr}")
      endif ()

    endforeach (txhdr ${TFP_HEADERS})

# Clean up and save list of include directories into the cache
    list(LENGTH ${txpkgreg}_INCLUDE_DIRS txinclistlen)
    if (${txinclistlen})
      list(REMOVE_DUPLICATES ${txpkgreg}_INCLUDE_DIRS)
# CMake conventions are that this is not a cache variable
      # set(${txpkgreg}_INCLUDE_DIRS
        # ${${txpkgreg}_INCLUDE_DIRS}
        # CACHE STRING "All include directories for ${txpkgreg}"
      # )
    else ()
      message(WARNING "None of ${TFP_HEADERS} found.  Define ${txpkguc}_DIR to find them.")
    endif ()
  endif ()

###########################################################################
#
# Look for LIBRARIES
# Finding none is fatal, but finding a subset is okay as allows us
# to look for a maximum set, as needed for Trilinos.
# XXX_LIBRARIES - NOT CACHED
#   The libraries to link against to use XXX. These should include full paths.
# XXX_YY_LIBRARY -
#   Name of YY library that is part of the XXX system.
#
###########################################################################

# Build list of search directories
  string(LENGTH "${TFP_LIBRARIES}" txlibslen)
  if (${txlibslen})
# Add in user-supplied subdirs
    string(LENGTH "${TFP_LIBRARY_SUBDIRS}" txlen)
    if (${txlen})
      set(txlibsubdirs ${TFP_LIBRARY_SUBDIRS})
    else ()
# Default subdirectory
      set(txlibsubdirs lib)
    endif ()

# Does anyone use this?  Should either move to argument list
# or remove entirely.  TODO - marc
# If LIBRARY_DIRS specified, add that to the front of the path
    if (${txpkgreg}_LIBRARY_DIRS)
      set(txlibsubdirs . ${txlibsubdirs})
      set(txpath ${${txpkgreg}_LIBRARY_DIRS} ${txpath})
    endif ()

    if (DEBUG_CMAKE)
      message(STATUS "Looking for libraries under ${txpath} with subdirs, ${txlibsubdirs}")
    endif ()

# Clear variables
    set(${txpkgreg}_LIBRARIES)
    set(${txpkgreg}_LIBRARY_DIRS)
    set(${txpkgreg}_LIBRARY_NAMES)
    set(${txpkgreg}_FOUND_SOME_LIBRARY FALSE)

# Look for each requested library
    foreach (txlib ${TFP_LIBRARIES})
# Build variable name of the form XXX_yyy_LIBRARY
      string(REGEX REPLACE "[./-]" "_" txlibvar ${txlib})
      set(txlibdirvar ${txpkgreg}_${txlibvar}_LIBRARY_DIR)
      unset(${txlibdirvar})
      set(txlibvar ${txpkgreg}_${txlibvar}_LIBRARY)
      unset(${txlibvar})

# First look in defined paths
      find_library(${txlibvar}
        ${txlib}
        PATHS ${txpath}
        PATH_SUFFIXES ${txlibsubdirs}
        NO_DEFAULT_PATH
      )
      if (DEBUG_CMAKE)
        message(STATUS "After initial search, ${txlibvar} = ${${txlibvar}}.")
      endif ()

# If not found, try again in default paths
      if (NOT ${txlibvar})
        find_library(${txlibvar}
          ${txlib}
          PATHS ${txpath}
          PATH_SUFFIXES ${txlibsubdirs}
        )
        if (DEBUG_CMAKE)
          message(STATUS "After second search, ${txlibvar} = ${${txlibvar}}.")
        endif ()
      endif ()

# Add to list of all libraries(if found)
      if (${txlibvar})
        if (DEBUG_CMAKE)
          message(STATUS "Found library ${txlib}: ${txlibvar} = ${${txlibvar}}")
        endif ()
        set(${txpkgreg}_FOUND_SOME_LIBRARY TRUE)
        set(${txpkgreg}_LIBRARIES
            ${${txpkgreg}_LIBRARIES}
            ${${txlibvar}}
        )
        get_filename_component(${txlibdirvar} ${${txlibvar}}/.. REALPATH)
        if (DEBUG_CMAKE)
          message(STATUS "${txlibdirvar} = ${${txlibdirvar}}")
        endif ()
        list(APPEND ${txpkgreg}_LIBRARY_DIRS ${${txlibdirvar}})
# NAME_WE removes from first '.'.  We need from last.
        get_filename_component(libname ${${txlibvar}} NAME)
        string(REGEX REPLACE "\\.[^\\.]*$" "" libname "${libname}")
        string(REGEX REPLACE "^lib" "" libname ${libname})
        list(APPEND ${txpkgreg}_LIBRARY_NAMES ${libname})
      else ()
# Yes, this uses the string WARNING instead of the macro argument WARNING
        message("WARNING - Library ${txlib} not found.")
      endif ()
# Add both to the cache
      # set(${txlibvar}
        # ${${txlibvar}}
        # CACHE FILEPATH "Location of ${txlib}"
      # )
      set(${txlibdirvar}
        ${${txlibdirvar}}
        CACHE PATH "Directory of ${txlib}"
      )
    endforeach (txlib ${TFP_LIBRARIES})
    if (DEBUG_CMAKE)
      message(STATUS "${txpkgreg}_LIBRARY_DIRS = ${${txpkgreg}_LIBRARY_DIRS}")
    endif ()

# Clean up and commit variables to cache
    list(LENGTH ${txpkgreg}_LIBRARIES txliblistlen)
    if (NOT ${txliblistlen})
      message("WARNING - None of the libraries, ${TFP_LIBRARIES}, found.  Define ${txpkguc}_DIR to find them.")
    else ()
      list(REMOVE_DUPLICATES "${txpkgreg}_LIBRARIES")
      list(REMOVE_DUPLICATES "${txpkgreg}_LIBRARY_DIRS")
# The first dir is the library dir
      list(GET "${txpkgreg}_LIBRARY_DIRS" 0 ${txpkgreg}_LIBRARY_DIR)
      if (NOT DEFINED ${txpkgreg}_DIR)
        get_filename_component(${txpkgreg}_DIR ${${txpkgreg}_LIBRARY_DIR}/.. REALPATH)
      endif ()
      if (DEBUG_CMAKE)
        message(STATUS "${txpkgreg}_DIR = ${${txpkgreg}_DIR}")
        message(STATUS "${txpkgreg}_LIBRARIES = ${${txpkgreg}_LIBRARIES}")
        message(STATUS "${txpkgreg}_LIBRARY_DIRS = ${${txpkgreg}_LIBRARY_DIRS}")
      endif ()
    endif ()

# Find static libraries
    SciGetStaticLibs("${${txpkgreg}_LIBRARIES}" ${txpkgreg}_STLIBS)

  endif ()

#################################################################
#
# On windows, we want to look for the dlls
# XXX_DLLS - CACHED
#   List of all found dlls including full path to each
# XXX_yyy_DLLS
#   Path to individual yyy library from XXX package
#  DOES NOT EXIST YET - TODO
#
#################################################################

  if (WIN32)
    if (DEBUG_CMAKE)
      message(STATUS "Looking for the DLL counterparts to all found libraries.")
    endif ()

# Clear variables
    set(${txpkgreg}_DLLS)
    set(${txpkgreg}_FOUND_SOME_DLL FALSE)

# Look for a dll for each found library
    foreach (foundlib ${${txpkgreg}_LIBRARIES})
      # get_filename_component(librootname ${foundlib} NAME_WE)
# NAME_WE removes from first '.'.  We need from last.
      get_filename_component(librootname ${foundlib} NAME)
      string(REGEX REPLACE "\\.[^\\.]*$" "" librootname "${librootname}")
# This assumes that dll is in "./lib/../bin"
      get_filename_component(libdir ${foundlib}/.. REALPATH)
      get_filename_component(dlldir1 "${libdir}/../bin" REALPATH)
      get_filename_component(dlldir2 "${libdir}/.." REALPATH)
      if (DEBUG_CMAKE)
        message(STATUS "Looking for DLL counterpart to ${foundlib} in ${dlldir1}/${librootname}.dll")
      endif ()
      if (EXISTS ${dlldir1}/${librootname}.dll)
        if (DEBUG_CMAKE)
          message(STATUS "Found DLL ${dlldir1}/${librootname}.dll")
        endif ()
        set(${txpkgreg}_DLLS ${${txpkgreg}_DLLS} ${dlldir1}/${librootname}.dll)
      else ()
        if (DEBUG_CMAKE)
          message(STATUS "Second chance: looking for DLL in ${dlldir2}/${librootname}.dll")
        endif ()
        if (EXISTS ${dlldir2}/${librootname}.dll)
          if (DEBUG_CMAKE)
            message(STATUS "Found DLL ${dlldir2}/${librootname}.dll")
          endif ()
          set(${txpkgreg}_DLLS ${${txpkgreg}_DLLS} ${dlldir2}/${librootname}.dll)
        else ()
          if (DEBUG_CMAKE)
            message(STATUS "Third chance: looking for DLL in ${libdir}/${librootname}.dll")
          endif ()
          if (EXISTS ${libdir}/${librootname}.dll)
            if (DEBUG_CMAKE)
              message(STATUS "Found DLL ${libdir}/${librootname}.dll")
            endif ()
            set(${txpkgreg}_DLLS ${${txpkgreg}_DLLS} ${libdir}/${librootname}.dll)
          else ()
            if (DEBUG_CMAKE)
              message(STATUS "${librootname} has no accompanying dll.")
            endif ()
          endif ()
        endif ()
      endif ()
    endforeach ()
  endif ()

####################################################################
#
# Determine if this package should be marked as FOUND or not
# At the moment, we mark it as found if ANYTHING was found
# Per http://www.cmake.org/Wiki/CMake:How_To_Find_Installed_Software,
# The convention is to capitalize the _FOUND variable.
#
####################################################################

  if (${txpkgreg}_FOUND_SOME_LIBRARY OR ${txpkgreg}_FOUND_SOME_HEADER OR
       ${txpkgreg}_FOUND_SOME_EXECUTABLE OR ${txpkgreg}_FOUND_SOME_DLL OR
       ${txpkgreg}_FOUND_SOME_FILE)
    set(${txpkguc}_FOUND TRUE)
    if (DEBUG_CMAKE OR NOT ${txpkgreg}_FIND_QUIETLY)
      message(STATUS "Found ${txpkgreg}.")
      sciPrintCMakeResults(${txpkgreg})
    endif ()
  else ()
    set(${txpkguc}_FOUND FALSE)
    if (DEBUG_CMAKE)
      message(STATUS "Failed to find package ${txpkgreg}")
      sciPrintCMakeResults(${txpkgreg})
    endif ()
# If this was marked as a required package, fail
    if (${txpkgreg}_FIND_REQUIRED)
      message(FATAL_ERROR "Unable to find required package ${txpkgreg} - failing")
    endif ()
  endif ()

  message("--------- SciFindPackage done with ${TFP_PACKAGE} -----------")
  message("")
  # if (DEBUG_CMAKE)
    message(STATUS "${txpkguc}_FOUND = ${${txpkguc}_FOUND}.")
  # endif ()

endmacro(SciFindPackage)

