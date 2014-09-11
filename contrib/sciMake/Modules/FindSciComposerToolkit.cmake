# - FindSciComposerToolkit: Module to find include directories and
#   libraries for ComposerToolkit.
#
# Module usage:
#   find_package(SciComposerToolkit ...)
#
# This module will define the following variables:
#  HAVE_COMPOSERTOOLKIT, COMPOSERTOOLKIT_FOUND = Whether libraries and includes are found
#  ComposerToolkit_INCLUDE_DIRS       = Location of ComposerToolkit includes
#  ComposerToolkit_LIBRARY_DIRS       = Location of ComposerToolkit libraries
#  ComposerToolkit_LIBRARIES          = Required libraries
#  ComposerToolkit_SHAREDIR           =
#  ComposerToolkit_RESOURCES_DIR      =

#################################################
#
# Find module for Composer Toolkit includes and lib
#
# $Id: FindSciComposerToolkit.cmake 1161 2011-12-17 15:44:00Z cary $
#
#################################################

if (NOT DEFINED ENABLE_VISIT)
  message(STATUS "ENABLE_VISIT not defined, using false.")
  set(ENABLE_VISIT false)
else ()
  message(STATUS "ENABLE_VISIT is ${ENABLE_VISIT}")
endif ()

if (ENABLE_VISIT)
  set(SHORTCUT_NAME composertoolkit-visit)
else ()
  set(SHORTCUT_NAME composertoolkit-novisit)
endif ()

message(STATUS "Looking for composertoolkit version ${SHORTCUT_NAME}")

# Build the list of executables to search for
set(REQUIRED_EXECUTABLES)
if (NOT WINDOWS)
  set(REQUIRED_EXECUTABLES
      "ctk_postbuild.sh"
      "runPostbuildScript.sh.in"
      )
endif ()

SciFindPackage(
        PACKAGE ComposerToolkit
        INSTALL_DIR "${SHORTCUT_NAME}"
        EXECUTABLES ${REQUIRED_EXECUTABLES}
        HEADERS CtkAppSpecifier.h
        LIBRARIES composertoolkit
)

if (COMPOSERTOOLKIT_FOUND)

  if (NOT WINDOWS)
    if (NOT ComposerToolkit_ctk_postbuild_sh)
      message(FATAL_ERROR "Could not find ctk_postbuild.sh")
    endif ()
    if (NOT ComposerToolkit_runPostbuildScript_sh_in)
      message(FATAL_ERROR "Could not find runPostbuildScript.sh.in")
    endif ()
  endif ()

  message(STATUS "Found ComposerToolkit")
  set(HAVE_COMPOSERTOOLKIT 1 CACHE BOOL "Whether have the ComposerToolkit library")
# Use ComposerToolkit_INCLUDE_DIR to derive COMPOSERTOOLKIT_DIR
# Which should be a single level higher in the directory structure
  get_filename_component(COMPOSERTOOLKIT_DIR "${ComposerToolkit_CtkAppSpecifier_h_INCLUDE_DIR}" PATH CACHE)
  message(STATUS "  COMPOSERTOOLKIT_DIR = ${COMPOSERTOOLKIT_DIR}")
  set(ComposerToolkit_SHAREDIR "${COMPOSERTOOLKIT_DIR}/share")
  message(STATUS "  ComposerToolkit_SHAREDIR = ${ComposerToolkit_SHAREDIR}")
  file(READ ${ComposerToolkit_SHAREDIR}/svninfo.txt svninfo)
  # message("svninfo = ${svninfo}.")
  string(REGEX MATCH "Last Changed Rev: [0-9]*" svnrev ${svninfo})
  # message("svnrev = ${svnrev}.")
  string(REGEX REPLACE "Last Changed Rev: " "" CTK_SVNREV ${svnrev})
  message(STATUS "  ComposerToolkit Revision = ${CTK_SVNREV}.")
  find_path(ComposerToolkit_RESOURCES_DIR ctkResources.qrc PATHS "${COMPOSERTOOLKIT_DIR}/resources")
  message(STATUS "ComposerToolkit_RESOURCES_DIR = ${ComposerToolkit_RESOURCES_DIR}")

else ()
  message(STATUS "Did not find ComposerToolkit. Use -DCOMPOSERTOOLKIT_DIR to specify the installation directory.")
  if (SciComposerToolkit_FIND_REQUIRED)
    message(FATAL_ERROR "Failed.")
  endif ()
endif ()


