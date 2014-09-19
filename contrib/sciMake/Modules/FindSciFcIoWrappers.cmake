# - FindSciFcIoWrappers: Module to find include directories and
#   libraries for Fc IO Wrappers
#
# Module usage:
#   find_package(SciFcIoWrappers ...)
#
# This module will define the following variables:
#  HAVE_FCIOWRAPPERS, FCIOWRAPPERS_FOUND = Whether libraries and includes are found
#  FcIoWrappers_INCLUDE_DIRS       = Location of Polyswift includes
#  FcIoWrappers_LIBRARY_DIRS       = Location of Polyswift libraries
#  FcIoWrappers_LIBRARIES          = Required libraries

######################################################################
#
# FindSciFcIoWrappers: find includes and libraries for Fcio Wrappers
#
# $Id: FindSciFcIoWrappers.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################


if (ENABLE_PARALLEL)
  set(instdir "fciowrappers-par;fciowrappers-ben")
else ()
  set(instdir "fciowrappers")
endif ()
SciFindPackage(PACKAGE "FcIoWrappers"
  INSTALL_DIR "${instdir}"
  HEADERS "vshdf5_dummy.h"
  MODULES "ezcdf;ezcdf_attrib;ezcdf_genget;ezcdf_genput;ezcdf_inqvar;ezcdf_opncls;hdf5_api"
  LIBRARIES "ezcdf;vshdf5"
  LIBRARY_SUBDIRS "lib"
)

if (FCIOWRAPPERS_FOUND)
  set(HAVE_FCIOWRAPPERS 1 CACHE BOOL "Whether have the FCIOWRAPPERS library")
  set(HAVE_FcIoWrappers 1 CACHE BOOL "Whether have the FCIOWRAPPERS library")
  include(${TXCMAKE_DIR}/SciGetDepsFromInstall.cmake)
# Get hdf5 libraries
  SciGetDepsFromInstall(Hdf5 ${FcIoWrappers_DIR} HDF5)
# Get netcdf libraries
  SciGetDepsFromInstall(Netcdf ${FcIoWrappers_DIR} NETCDF)
endif ()
message(STATUS "Netcdf_FOUND = ${Netcdf_FOUND}.")
message(STATUS "NETCDF_FOUND = ${NETCDF_FOUND}.")
message(STATUS "Hdf5_FOUND = ${Hdf5_FOUND}.")
message(STATUS "HDF5_FOUND = ${HDF5_FOUND}.")

