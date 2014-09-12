# - FindSciBabel: Module to find include directories and libraries
#   for Babel. This module was implemented as there is no stock
#   CMake module for Babel.
#
# This module can be included in CMake builds in find_package:
#   find_package(SciBabel REQUIRED)
#
# This module will define the following variables:
#  HAVE_BABEL         = Whether have the Babel library
#  Babel_INCLUDE_DIRS = Location of Babel includes
#  Babel_LIBRARY_DIRS = Location of Babel libraries
#  Babel_LIBRARIES    = Required libraries
#  Babel_STLIBS       = Location of Babel static library

######################################################################
#
# FindSciBabel: find includes and libraries for Babel
#
# $Id: FindSciBabel.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

# Find shared libs
SciFindPackage(PACKAGE "BabelShared"
  INSTALL_DIR "babel-shared"
  HEADERS "sidl.h;sidl.hxx;sidl.inc"
  INCLUDE_SUBDIRS "include/c;include/cxx;include/f77;include/python2.6/llnl_babel;include/$PYDIR/babel"
  LIBRARIES "sidlstub_cxx;sidlstub_f90;sidl;chasmlite"
  MODULES "sidl"
  MODULE_SUBDIRS "include/f90"
)
if (BABELSHARED_FOUND)
  set(HAVE_BABEL_SHARED 1 CACHE BOOL "Whether have the BABEL_SHARED library")
endif ()

# Find static libs
SciFindPackage(PACKAGE "BabelStatic"
  INSTALL_DIR "babel-static"
  HEADERS "sidl.h;sidl.hxx;sidl.inc"
  INCLUDE_SUBDIRS "include/c;include/cxx;include/f77"
  LIBRARIES "sidlstub_cxx;sidlstub_f90;sidl;chasmlite"
  MODULES "sidl"
  MODULE_SUBDIRS "include/f90"
)
if (BABELSTATIC_FOUND)
  set(HAVE_BABEL_STATIC 1 CACHE BOOL "Whether have the BABEL_STATIC library")
endif ()
# Make sure these are static
SciGetStaticLibs("${BabelStatic_LIBRARIES}" BabelStatic_LIBRARIES)

# Combined parameters
if (BABELSHARED_FOUND OR BABELSTATIC_FOUND)
  set(HAVE_ANY_BABEL 1 CACHE BOOL "Whether have any BABEL library")
endif ()
if (BABELSHARED_FOUND AND BABELSTATIC_FOUND)
  set(HAVE_BOTH_BABEL 1 CACHE BOOL "Whether have both BABEL libraries")
endif ()

