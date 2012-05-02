# - Find PKG_CONFIG
# This module looks for pkg-config. This module defines the 
# following values:
#  PKG_CONFIG_EXECUTABLE: the full path to the pkg-config tool.
#  PKG_CONFIG_FOUND: True if pkg-config has been found.

#=============================================================================
# Copyright 2001-2009 Kitware, Inc.
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

INCLUDE(FindCygwin)

FIND_PROGRAM(PKG_CONFIG_EXECUTABLE
  pkg-config
  ${CYGWIN_INSTALL_PATH}/bin
)

# handle the QUIETLY and REQUIRED arguments and set PKG_CONFIG_FOUND to TRUE if 
# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(PKG_CONFIG DEFAULT_MSG PKG_CONFIG_EXECUTABLE)

MARK_AS_ADVANCED( PKG_CONFIG_EXECUTABLE )

# PKG_CONFIG option is deprecated.
# use PKG_CONFIG_EXECUTABLE instead.
SET (PKG_CONFIG ${PKG_CONFIG_EXECUTABLE} )
