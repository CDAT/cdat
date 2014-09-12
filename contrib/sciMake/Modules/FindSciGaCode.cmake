# - FindSciGaCode: Module to find include directories and
#   libraries for GaCode
#
# Module usage:
#   find_package(SciGaCode ...)
#
# This module will define the following variables:
#  HAVE_GACODE, GACODE_FOUND = Whether libraries and includes are found
#  GaCode_INCLUDE_DIRS       = Location of Polyswift includes
#  GaCode_LIBRARY_DIRS       = Location of Polyswift libraries
#  GaCode_LIBRARIES          = Required libraries

######################################################################
#
# FindSciGaCode: find includes and libraries for gacode
#
# $Id: FindSciGaCode.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdir "gacode-par")
  set(GaCode_INST_MODS gyro_interface neo_interface tglf_interface)
  set(GaCode_INST_LIBS gacodegyro gacodeneo gacodetglf gacodeshared)
else ()
  set(instdir "gacode")
  set(GaCode_INST_MODS tglf_interface)
  set(GaCode_INST_LIBS gacodetglf gacodeshared)
endif ()

SciFindPackage(PACKAGE GaCode
  INSTALL_DIRS ${instdir}
  MODULES ${GaCode_INST_MODS}
  LIBRARIES ${GaCode_INST_LIBS}
)

if (GACODE_FOUND)
  set(HAVE_GACODE 1 CACHE BOOL "Whether have the gacode library")
  set(HAVE_GaCode 1 CACHE BOOL "Whether have the gacode library")
  include(${TXCMAKE_DIR}/SciGetDepsFromInstall.cmake)
# Get fciowrappers libraries
  SciGetDepsFromInstall(Fciowrappers ${GaCode_DIR} FCIOWRAPPERS)
# Get hdf5 libraries
#  SciGetDepsFromInstall(Hdf5 ${FcIoWrappers_DIR} HDF5)
# Get netcdf libraries
#  SciGetDepsFromInstall(Netcdf ${FcIoWrappers_DIR} NETCDF)

  # This should be improved with if statements.
  set(HAVE_TGLF 1 CACHE BOOL "Whether gacode has TGLF")
  set(HAVE_Tglf 1 CACHE BOOL "Whether gacode has TGLF")
  if (ENABLE_PARALLEL)
     set(HAVE_GYRO 1 CACHE BOOL "Whether gacode has GYRO")
     set(HAVE_Gyro 1 CACHE BOOL "Whether gacode has GYRO")
  endif ()
endif ()

