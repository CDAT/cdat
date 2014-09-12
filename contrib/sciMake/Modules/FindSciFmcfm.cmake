# - FindSciFmcfm: Module to find include directories and
#   libraries for Fmcfm.
#
# Module usage:
#   find_package(SciFmcfm ...)
#
# This module will define the following variables:
#  HAVE_FMCFM, FMCFM_FOUND = Whether libraries and includes are found
#  Fmcfm_INCLUDE_DIRS       = Location of Fmcfm includes
#  Fmcfm_LIBRARY_DIRS       = Location of Fmcfm libraries
#  Fmcfm_LIBRARIES          = Required libraries

######################################################################
#
# FindSciFmcfm: find includes and libraries for Fmcfm
#
# $Id: FindSciFmcfm.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdir fmcfm-par)
else ()
  set(instdir fmcfm)
endif ()
SciFindPackage(PACKAGE "Fmcfm"
  INSTALL_DIR "${instdir}"
  HEADERS "FmGlf23TransportModel.h;FmNclassTransportModel.h;FmGyroTransportModel.h;FmTglfTransportModel.h"
  LIBRARIES "fmcfmcppwrap;fmcfmcxx;fmcfmwrap;fmcfm"
  LIBRARY_SUBDIRS "lib/${Fortran_COMP_LIB_SUBDIR};lib"
)

if (FMCFM_FOUND)
  # message(STATUS "Found FMCFM")
  set(HAVE_FMCFM 1 CACHE BOOL "Whether have the FMCFM library")
# Find gacodes
  include(${TXCMAKE_DIR}/SciGetDepsFromInstall.cmake)
# Get gacodes libraries
  SciGetDepsFromInstall(GaCode ${Fmcfm_DIR} GACODE)
# Get ntcctransport libraries
  SciGetDepsFromInstall(NtccTransport ${Fmcfm_DIR} NTCCTRANSPORT)
endif ()




