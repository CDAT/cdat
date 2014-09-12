# - FindSciNetlibLite: Module to find include directories and
#   libraries for NetlibLite
#
# Module usage:
#   find_package(SciNetlibLite ...)
#
# This module will define the following variables:
#  HAVE_NETLIBLITE, NETLIBLITE_FOUND = Whether libraries and includes are found
#  NetlibLite_INCLUDE_DIRS       = Location of Polyswift includes
#  NetlibLite_LIBRARY_DIRS       = Location of Polyswift libraries
#  NetlibLite_LIBRARIES          = Required libraries

######################################################################
#
# FindNetlibLite: find includes and libraries for txbase
#
# $Id: FindSciNetlibLite.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

if (ENABLE_PARALLEL)
  set(instdirs netlib_lite-ben netlib_lite)
else ()
  set(instdirs netlib_lite)
endif ()

SciFindPackage(PACKAGE "NetlibLite"
  INSTALL_DIRS ${instdirs}
  MODULES lsode_mod
  LIBRARIES lsode nlother r8slatec
)

