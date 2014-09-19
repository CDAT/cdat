# - FindSciNsis: Module to find include directories and
#   libraries for Nsis.
#
# Module usage:
#   find_package(SciNsis ...)
#
# May need to be changed to use SciFindPackage()

#################################################
#
# Find NSIS packager
#
# $Id: FindSciNsis.cmake 1161 2011-12-17 15:44:00Z cary $
#
#################################################

if (WIN32)
  set(TXC_NSIS_SEARCHPATH
    "$ENV{PROGRAMFILES}/NSIS" "$ENV{PROGRAMFILES(X86)}/NSIS"
  )
endif ()

find_program(MAKENSIS
  makensis
  PATHS ${TXC_NSIS_SEARCHPATH}
  DOC "Location of the NSIS executable"
)
if (MAKENSIS)
  set(MAKENSIS_FOUND TRUE)
endif ()

