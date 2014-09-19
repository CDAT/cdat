# - FindSciNtccTransport: Module to find include directories and
#   libraries for Ntcc Transport
#
# Module usage:
#   find_package(SciNtccTransport ...)
#
# This module will define the following variables:
#  HAVE_NTCCTRANSPORT, NTCCTRANSPORT_FOUND = Whether libraries and includes are found
#  NtccTransport_INCLUDE_DIRS       = Location of Polyswift includes
#  NtccTransport_LIBRARY_DIRS       = Location of Polyswift libraries
#  NtccTransport_LIBRARIES          = Required libraries

######################################################################
#
# FindSciNtccTransport: find includes and libraries for Ntcc Transport
#
# $Id: FindSciNtccTransport.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdir "ntcc_transport-par")
  set(GLF23_INST_LIB glf23mpi)
else ()
  set(instdir "ntcc_transport")
  set(GLF23_INST_LIB glf23)
endif ()

SciFindPackage(PACKAGE NtccTransport
  INSTALL_DIRS ${instdir}
  MODULES glf23_data_mod nclass_interface
  LIBRARIES ${GLF23_INST_LIB} mmm95 ifspppl nclass kapisn mmm71
)

if (NTCCTRANSPORT_FOUND)
  set(HAVE_NTCCTRANSPORT 1 CACHE BOOL "Whether have the ntcc_transport library")
  set(HAVE_NtccTransport 1 CACHE BOOL "Whether have ntcc_transport the library")
endif ()

