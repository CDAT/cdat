# - FindSciLicMgr: Module to find include directories and
#   libraries for SciLicMgr.
#
# Module usage:
#   find_package(SciLicMgr ...)
#
# This module will define the following variables:
#  HAVE_TXLICMGR, TXLICMGR_FOUND = Whether libraries and includes are found
#  SciLicMgr_INCLUDE_DIRS       = Location of SciLicMgr includes
#  SciLicMgr_LIBRARY_DIRS       = Location of SciLicMgr libraries
#  SciLicMgr_LIBRARIES          = Required libraries

######################################################################
#
# FindSciLicMgr: find includes and libraries for txlicmgr
#
# $Id: FindSciLicMgr.cmake 1161 2011-12-17 15:44:00Z cary $
#
# Copyright &copy; 2011 Tech-X Corporation.
#
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (WIN32)
  set(SciLicMgr_statlib txlicmgr.lib)
else ()
  set(SciLicMgr_statlib libtxlicmgr.a)
endif ()

if (HAVE_MPI)
  SciFindPackage(PACKAGE "SciLicMgr"
                INSTALL_DIR "txlicmgr-par"
                HEADERS "SciLicenseMgr.h"
                LIBRARIES "${SciLicMgr_statlib}"
                LIBRARY_SUBDIRS "lib"
  )
else ()
  SciFindPackage(PACKAGE "SciLicMgr"
                 HEADERS "SciLicenseMgr.h"
                LIBRARIES "${SciLicMgr_statlib}"
                LIBRARY_SUBDIRS "lib"
  )
endif ()

