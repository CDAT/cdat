# - FindSciNubeam: Module to find include directories and libraries
#   for Nubeam. This module was implemented as there is no stock
#   CMake module for Nubeam.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciNubeam REQUIRED)
#
# This module will define the following variables:
#  HAVE_NUBEAM         = Whether have the Nubeam library
#  Nubeam_INCLUDE_DIRS = Location of Nubeam includes
#  Nubeam_LIBRARY_DIRS = Location of Nubeam libraries
#  Nubeam_LIBRARIES    = Required libraries
#  Nubeam_STLIBS       = Location of Nubeam static library

######################################################################
#
# FindSciNubeam: find includes and libraries for nubeam
#
# $Id: FindSciNubeam.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################


if (ENABLE_PARALLEL)
  set(instdir nubeam-par)
else ()
  set(instdir nubeam)
endif ()

# Currently we are not working about nubeam-serial because of build
# problems
if (ENABLE_PARALLEL)
SciFindPackage(PACKAGE "Nubeam"
  INSTALL_DIR "${instdir}"
  HEADERS "nubeam_svnversion.h;nubeam.h"
  LIBRARIES "TranspPhage;TranspGraphic2"
  # LIBRARIES "TranspPhage"
  LIBRARY_SUBDIRS "lib"
)
endif ()

set(NUBEAM_DIR ${Nubeam_DIR})

if (NUBEAM_FOUND)
  set(HAVE_NUBEAM 1 CACHE BOOL "Whether have the Nubeam library")
endif ()

